/**
 * default ip2region implementation
 *
 * @see        #ip2region.h
 * @author    chenxin<chenxin619315@gmail.com>
 * @date    2015-10-30
*/

#include "ip2region.h"
#include <stdio.h>
#include <stdlib.h>

/**
 * create a new ip2region object
 *
 * @param    dbFile path
*/
IP2R_API uint_t ip2region_create(ip2region_t ip2rObj, char *dbFile)
{
    memset(ip2rObj, 0x00, sizeof(ip2region_entry));
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

    //open the db file
    ip2rObj->dbHandler = fopen(dbFile, "rb");
    if ( ip2rObj->dbHandler == NULL  ) {
        IP2R_FREE(ip2rObj->HeaderSip);
        IP2R_FREE(ip2rObj->HeaderPtr);
        //fprintf(stderr, "Fail to open the db file %s\n", ip2rObj>dbFile);
        //exit(-1);
        return 0;
    }

    ip2rObj->firstIndexPtr = 0;
    ip2rObj->lastIndexPtr  = 0;
    ip2rObj->totalBlocks   = 0;
    ip2rObj->dbBinStr      = NULL;

    return 1;
}

/**
 * destroy the specifield ip2region object
 *
 * @param    ip2region_t
*/
IP2R_API uint_t ip2region_destroy(ip2region_t ip2rObj)
{
    IP2R_FREE(ip2rObj->HeaderSip);
    ip2rObj->HeaderSip = NULL;
    IP2R_FREE(ip2rObj->HeaderPtr);
    ip2rObj->HeaderPtr = NULL;

    //close the db file resource
    if ( ip2rObj->dbHandler != NULL ) {
        fclose(ip2rObj->dbHandler);
        ip2rObj->dbHandler = NULL;
    }

    //free the db binary string
    if ( ip2rObj->dbBinStr != NULL ) {
        IP2R_FREE(ip2rObj->dbBinStr);
        ip2rObj->dbBinStr = NULL;
    }

    return 1;
}

/**
 * get the region associated with the specified ip address with the memory binary search algorithm
 *
 * @param   ip2rObj
 * @param   ip
 * @param   datablock
*/
IP2R_API uint_t ip2region_memory_search(ip2region_t ip2rObj, uint_t ip, datablock_t datablock) 
{
    int l, h, m, p;
    uint_t sip, eip, dptr;
    int dataLen, dataptr;
    long filesize;
    char *buffer;

    if ( ip2rObj->dbBinStr == NULL ) {
        //get the size of the file
        fseek(ip2rObj->dbHandler, 0, SEEK_END);
        filesize = ftell(ip2rObj->dbHandler);
        fseek(ip2rObj->dbHandler, 0, SEEK_SET);

        //alloc the buffer size
        ip2rObj->dbBinStr = IP2R_MALLOC(filesize);
        if ( ip2rObj->dbBinStr == NULL ) {
            return 0;
        }

        //now read the whole file
        if ( fread(ip2rObj->dbBinStr, filesize, 1, ip2rObj->dbHandler) != 1 ) {
            return 0;
        }

        buffer = ip2rObj->dbBinStr;
        ip2rObj->firstIndexPtr = getUnsignedInt(buffer, 0);
        ip2rObj->lastIndexPtr  = getUnsignedInt(buffer, 4);
        ip2rObj->totalBlocks   = (ip2rObj->lastIndexPtr-ip2rObj->firstIndexPtr)/INDEX_BLOCK_LENGTH + 1;
    }

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

IP2R_API uint_t ip2region_memory_search_string(ip2region_t ip2rObj, char *ip, datablock_t datablock)
{
    return ip2region_memory_search(ip2rObj, ip2long(ip), datablock);
}

/**
 * get the region associated with the specifield ip address with binary search algorithm
 *
 * @param    ip2rObj
 * @param    ip
 * @param    datablock
 * @return    uint_t
*/
IP2R_API uint_t ip2region_binary_search(ip2region_t ip2rObj, uint_t ip, datablock_t datablock)
{
    int l, h, m, p;
    uint_t sip, eip, dptr;
    char buffer[256];
    int dataLen, dataptr;

    if ( ip2rObj->totalBlocks == 0 ) {
        fseek(ip2rObj->dbHandler, 0, 0);
        if ( fread(buffer, 8, 1, ip2rObj->dbHandler) != 1 ) {
            return 0;
        }

        ip2rObj->firstIndexPtr = getUnsignedInt(buffer, 0);
        ip2rObj->lastIndexPtr  = getUnsignedInt(buffer, 4);
        ip2rObj->totalBlocks   = (ip2rObj->lastIndexPtr-ip2rObj->firstIndexPtr)/INDEX_BLOCK_LENGTH + 1;
    }

    //binary search the index blocks to define the data block
    l = 0; h = ip2rObj->totalBlocks; dptr = 0;
    while ( l <= h ) {
        m = (l + h) >> 1;
        p = ip2rObj->firstIndexPtr + m * INDEX_BLOCK_LENGTH;

        fseek(ip2rObj->dbHandler, p, 0);
        if ( fread(buffer, INDEX_BLOCK_LENGTH, 1, ip2rObj->dbHandler) != 1 ) {
            return 0;
        }

        sip = getUnsignedInt(buffer, 0);
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

    //memset(data, 0x00, sizeof(data));
    fseek(ip2rObj->dbHandler, dataptr, 0);
    if ( fread(buffer, dataLen, 1, ip2rObj->dbHandler) != 1 ) {
        return 0;
    }

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
    int i, idx;
    int l, m, h, p, sptr, eptr, indexBlockLen, dataLen, dataptr;
    uint_t sip, eip, idxptr, dptr;
    char buffer[TOTAL_HEADER_LENGTH];

    if ( ip2rObj->headerLen == 0 ) {
        idx = 0;
        fseek(ip2rObj->dbHandler, 8, 0);    //pass the super block
        if ( fread(buffer, TOTAL_HEADER_LENGTH, 1, ip2rObj->dbHandler) != 1 ) {
            return 0;
        }

        for ( i = 0; i < TOTAL_HEADER_LENGTH; i += 8 ) {
            sip    = getUnsignedInt(buffer, i);
            idxptr = getUnsignedInt(buffer, i + 4);
            if ( idxptr == 0 ) break;

            ip2rObj->HeaderSip[idx] = sip;
            ip2rObj->HeaderPtr[idx] = idxptr;
            idx++;
        }

        ip2rObj->headerLen = idx;
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
    fseek(ip2rObj->dbHandler, sptr, 0);
    if ( fread(buffer, indexBlockLen + INDEX_BLOCK_LENGTH, 1, ip2rObj->dbHandler) != 1 ) {
        return 0;
    }

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

    fseek(ip2rObj->dbHandler, dataptr, 0);
    if ( fread(buffer, dataLen, 1, ip2rObj->dbHandler) != 1 ) {
        return 0;
    }

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
