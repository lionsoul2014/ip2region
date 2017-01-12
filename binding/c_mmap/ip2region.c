/**
 * ip2region implementation with mmap support created by Leo Ma
 *
 * @see     #ip2region.h
 * @author  chenxin<chenxin619315@gmail.com>
 * @author  Leo Ma<http://git.oschina.net/begeekmyfriend>
 * @date    2017/01/12
*/

#include "ip2region.h"
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/mman.h>

/**
 * create a new ip2region object
 *
 * @param   dbFile path
*/
IP2R_API uint_t ip2region_create(ip2region_t ip2rObj, char *dbFile)
{
    struct stat st;

    memset(ip2rObj, 0x00, sizeof(ip2region_entry));
    ip2rObj->dbFile = IP2R_MALLOC(1024);
    strcpy(ip2rObj->dbFile, dbFile);

    int fd = open(dbFile, O_RDONLY);
    if ( fd < 0 ) {
        //fprintf(stderr, "Fail to open the db file %s\n", ip2rObj->dbFile);
        return 0;
    }

    if ( fstat(fd, &st) < 0 ) {
        //fprintf(stderr, "Fail to stat the db file %s\n", ip2rObj->dbFile);
        return 0;
    }

    ip2rObj->dbSize = st.st_size;
    ip2rObj->dbBinStr = mmap(NULL, ip2rObj->dbSize, PROT_READ, MAP_PRIVATE, fd, 0);
    if ( ip2rObj->dbBinStr == (void *) -1 ) {
        //fprintf(stderr, "Fail to map the db file %s\n", ip2rObj->dbFile);
        return 0;
    }

    // Close db file as soon as possible.
    close(fd);

    ip2rObj->headerLen = 0;
    ip2rObj->HeaderSip = (uint_t *) IP2R_MALLOC(TOTAL_HEADER_LENGTH);
    if ( ip2rObj->HeaderSip == NULL ) {
        return 0;
    }

    ip2rObj->HeaderPtr = (uint_t *) IP2R_MALLOC(TOTAL_HEADER_LENGTH);
    if ( ip2rObj->HeaderPtr == NULL ) {
        IP2R_FREE(ip2rObj->HeaderSip);
        return 0;
    }

    ip2rObj->firstIndexPtr = getUnsignedInt(ip2rObj->dbBinStr, 0);
    ip2rObj->lastIndexPtr  = getUnsignedInt(ip2rObj->dbBinStr, 4);
    ip2rObj->totalBlocks   = (ip2rObj->lastIndexPtr-ip2rObj->firstIndexPtr)/INDEX_BLOCK_LENGTH + 1;

    return 1;
}

/**
 * destroy the specifield ip2region object
 *
 * @param    ip2region_t
*/
IP2R_API uint_t ip2region_destroy(ip2region_t ip2rObj)
{
    IP2R_FREE(ip2rObj->dbFile);
    IP2R_FREE(ip2rObj->HeaderSip);
    IP2R_FREE(ip2rObj->HeaderPtr);

    //free the db binary string
    if ( ip2rObj->dbBinStr != NULL ) {
        munmap(ip2rObj->dbBinStr, ip2rObj->dbSize);
    }

    memset(ip2rObj, 0x00, sizeof(ip2region_entry));

    return 1;
}

/**
 * get the region associated with the specified ip address with the memory binary search algorithm
 *
 * @param   ip2rObj
 * @param   ip
 * @param   datablock
*/
IP2R_API uint_t ip2region_binary_search(ip2region_t ip2rObj, uint_t ip, datablock_t datablock) 
{
    int l, h, m, p;
    uint_t sip, eip, dptr;
    int dataLen, dataptr;
    char *buffer;

    l = 0; h = ip2rObj->totalBlocks; dptr = 0;
    while ( l <= h ) {
        m = (l + h) >> 1;
        p = ip2rObj->firstIndexPtr + m * INDEX_BLOCK_LENGTH;

        buffer = ip2rObj->dbBinStr + p;
        sip    = getUnsignedInt(buffer, 0);
        if ( ip < sip ) {
            h = m - 1;
        } else {
            eip = getUnsignedInt(buffer, 4);
            if ( ip > eip ) {
                l = m + 1;
            } else {
                dptr = getUnsignedInt(buffer, 8);
                break;
            }
        }
    }

    if ( dptr == 0 ) return 0;

    //get the data
    dataLen = ((dptr >> 24) & 0xFF);
    dataptr = (dptr & 0x00FFFFFF);
    buffer  = ip2rObj->dbBinStr + dataptr;

    //fill the data to the datablock
    datablock->city_id = getUnsignedInt(buffer, 0);
    dataLen -= 4;    //reduce the length of the city_id
    memcpy(datablock->region, buffer + 4, dataLen); 
    datablock->region[dataLen] = '\0';

    return 1;
}

IP2R_API uint_t ip2region_binary_search_string(ip2region_t ip2rObj, char *ip, datablock_t datablock)
{
    return ip2region_binary_search(ip2rObj, ip2long(ip), datablock);
}

/**
 * get the region associated with the specifield ip address with b-tree algorithm
 *
 * @param    ip2rObj
 * @param    ip
 * @param    datablock
 * @return    uint_t
*/
IP2R_API uint_t ip2region_btree_search(ip2region_t ip2rObj, uint_t ip, datablock_t datablock)
{
    int i, j;
    int l, m, h, p, sptr, eptr, indexBlockLen, dataLen, dataptr;
    uint_t sip, eip, idxptr, dptr;
    char *buffer;

    if ( ip2rObj->headerLen == 0 ) {
        buffer = ip2rObj->dbBinStr + 8;
        for ( i = j = 0; i < TOTAL_HEADER_LENGTH; i += 8, j++ ) {
            sip    = getUnsignedInt(buffer, i);
            idxptr = getUnsignedInt(buffer, i + 4);
            if ( idxptr == 0 ) break;

            ip2rObj->HeaderSip[j] = sip;
            ip2rObj->HeaderPtr[j] = idxptr;
        }

        ip2rObj->headerLen = j;
    }

    //search the header block to define the index block
    l = 0; h = ip2rObj->headerLen; sptr = 0; eptr = 0;
    while ( l <= h ) {
        m = ((l + h) >> 1);

        //perfetc matched, just return it
        if ( ip == ip2rObj->HeaderSip[m] ) {
            if ( m > 0 ) {
                sptr = ip2rObj->HeaderPtr[m-1];
                eptr = ip2rObj->HeaderPtr[m  ];
            } else {
                sptr = ip2rObj->HeaderPtr[m ];
                eptr = ip2rObj->HeaderPtr[m+1];
            }

            break;
        }

        //less then the middle value
        if ( ip < ip2rObj->HeaderSip[m] ) {
            if ( m == 0 ) {
                sptr = ip2rObj->HeaderPtr[m  ];
                eptr = ip2rObj->HeaderPtr[m+1];
                break;
            } else if ( ip > ip2rObj->HeaderSip[m-1] ) {
                sptr = ip2rObj->HeaderPtr[m-1];
                eptr = ip2rObj->HeaderPtr[m  ];
                break;
            }
            h = m - 1;
        } else {
            if ( m == ip2rObj->headerLen - 1 ) {
                sptr = ip2rObj->HeaderPtr[m-1];
                eptr = ip2rObj->HeaderPtr[m  ];
                break;
            } else if ( ip <= ip2rObj->HeaderSip[m+1] ) {
                sptr = ip2rObj->HeaderPtr[m  ];
                eptr = ip2rObj->HeaderPtr[m+1];
                break;
            }
            l = m + 1;
        }
    }

    //not matched just stop it
    if ( sptr == 0 ) return 0;

    indexBlockLen = eptr - sptr;
    buffer = ip2rObj->dbBinStr + sptr;

    dptr = 0; l = 0; h = indexBlockLen / INDEX_BLOCK_LENGTH;
    while ( l <= h ) {
        m = ((l + h) >> 1);
        p = m * INDEX_BLOCK_LENGTH;
        sip = getUnsignedInt(buffer, p);
        if ( ip < sip ) {
            h = m - 1;
        } else {
            eip = getUnsignedInt(buffer, p + 4);
            if ( ip > eip ) {
                l = m + 1;
            } else {
                dptr = getUnsignedInt(buffer, p + 8);
                break;
            }
        }
    }

    if ( dptr == 0 ) return 0;

    dataLen = ((dptr >> 24) & 0xFF);
    dataptr = (dptr & 0x00FFFFFF);
    buffer = ip2rObj->dbBinStr + dataptr;

    datablock->city_id = getUnsignedInt(buffer, 0);
    dataLen -= 4;
    memcpy(datablock->region, buffer + 4, dataLen);
    datablock->region[dataLen] = '\0';

    return 1;
}

IP2R_API uint_t ip2region_btree_search_string(ip2region_t ip2rObj, char *ip, datablock_t datablock)
{
    return ip2region_btree_search(ip2rObj, ip2long(ip), datablock);
}

/**
 * get a unsinged long(4bytes) from a specifield buffer start from the specifield offset
 *
 * @param    buffer
 * @param    offset
 * @return    uint_t
*/
IP2R_API uint_t getUnsignedInt(char *buffer, int offset)
{
    return (
        ((buffer[offset  ]) & 0x000000FF) | 
        ((buffer[offset+1] <<  8) & 0x0000FF00) | 
        ((buffer[offset+2] << 16) & 0x00FF0000) |
        ((buffer[offset+3] << 24) & 0xFF000000)
    );
}

/**
 * string ip to long
 *
 * @param    ip
 * @return    uint_t
*/
IP2R_API uint_t ip2long(char *ip)
{
    int i = 0, p = 24;
    char buffer[4], *cs = ip;
    uint_t ipval = 0;

    while ( *cs != '\0' ) {
        if ( *cs == '.' ) {
            //single part length limit
            if ( i > 3 ) {
                ipval = 0;
                break;
            }

            if ( p < 0 ) break;
            buffer[i] = '\0';
            ipval |= (atoi(buffer) << p);
            p -= 8;
            i = 0;
        } else {
            buffer[i++] = *cs;
        }

        cs++;
    }

    //append the rest parts
    if ( i > 3 ) return 0;
    buffer[i] = '\0';
    ipval |= atoi(buffer);

    return ipval;
}

/**
 * long to string ip
 *
 * @param    ip
 * @param    buffer
 * @return    uint_t(1 for success and 0 for failed)
*/
IP2R_API uint_t long2ip(uint_t ip, char *buffer)
{
    return 0;
}
