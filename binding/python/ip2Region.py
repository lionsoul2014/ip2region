#-*- coding:utf-8 -*-
"""
" ip2region python seacher client module
"
" Autho: koma<komazhang@foxmail.com>
" Date : 2015-11-06
"""
import struct, io, socket, sys, os

class Ip2Region(object):
	__headerSip = []
	__headerPtr = []
	__f			= None

	def __init__(self, dbfile):
		self.initDatabase(dbfile)

	def binarySearch(self, ip):
		"""
		" binary search method
		" param: ip
		"""
		if not ip.isdigit(): ip = self.ip2long(ip)

		print "binary...", ip

	def btreeSearch(self, ip):
		"""
		" b-tree search method
		" param: ip
		"""
		if not ip.isdigit(): ip = self.ip2long(ip)

		headerLen 		 = len(self.__headerSip) - 1
		l, h, sptr, eptr = (0, headerLen, 0, 0)
		while l <= h:
			m = int((l+h)/2)

			if ip == self.__headerSip[m]:
				if m > 0:
					sptr = self.__headerPtr[m-1]
					eptr = self.__headerPtr[m]
					break;
				else:
					sptr = self.__headerPtr[m]
					eptr = self.__headerPtr[m+1]
					break;

			if ip > self.__headerSip[m]:
				if m == headerLen:
					sptr = self.__headerPtr[m-1]
					eptr = self.__headerPtr[m]
					break;
				elif ip < self.__headerSip[m+1]:
					sptr = self.__headerPtr[m]
					eptr = self.__headerPtr[m+1]
					break;

				l = m + 1
			else:
				if m == 0:
					sptr = self.__headerPtr[m]
					eptr = self.__headerPtr[m+1]
					break;
				elif ip > self.__headerSip[m-1]:
					sptr = self.__headerPtr[m-1]
					eptr = self.__headerPtr[m]
					break;

				h = m - 1

		if sptr == 0: return "N1"

		indexLen = eptr - sptr
		self.__f.seek(sptr)
		b = self.__f.read(indexLen + 12)
		
		l, h, mixPtr = (0, int(indexLen/12), 0)
		while l <= h:
			m = int((l+h)/2)
			offset = m * 12

			if ip > self.getLong(b, offset):
				if ip > self.getLong(b, offset+4):
					l = m + 1
				else:
					mixPtr = self.getLong(b, offset+8)
					break;
			else:
				h = m - 1

		if mixPtr == 0: return "N2"

		dataPtr = mixPtr & 0x00FFFFFFL
		dataLen = (mixPtr >> 24) & 0xFF
		
		self.__f.seek(dataPtr)
		data = self.__f.read(dataLen)
		return {
			"city_id": self.getLong(data, 0),
			"region" : data[4:]
		}

	def initDatabase(self, dbfile):
		"""
		" initialize the database for search
		" param: dbFile
		"""
		try:
			self.__f = io.open(dbfile, "rb")
			#pass the super block
			self.__f.seek(8)
			#read the header block
			b = self.__f.read(4086)
			#parse the header block
			sip = None
			ptr = None
			for i in range(0, len(b)-1, 8):
				sip = self.getLong(b, i)
				ptr = self.getLong(b, i+4)
				if ptr == 0:
					break
				self.__headerSip.append(sip)
				self.__headerPtr.append(ptr)

		except IOError, e:
			print "[Error]: ", e
			sys.exit()

	def ip2long(self, ip):
		_ip = socket.inet_aton(ip)
		return struct.unpack("!L", _ip)[0]

	def isip(self, ip):
		p = ip.split(".")

		if len(p) != 4		   : return False
		for pp in p:
			if not pp.isdigit(): return False
			if len(pp) > 3	   : return False
			if int(pp) > 255   : return False

		return True

	def getLong(self, b, offset):
		if len( b[offset:offset+4] ) == 4:
			return struct.unpack('I', b[offset:offset+4])[0]
		return 0

	def close(self):
		self.__headerSip = None
		self.__headerPtr = None
		self.__f.close()
		self.__f		 = None

def testSearch():
	"""
	" ip2region test method
	"""
	llen = len(sys.argv)

	if llen < 2:
		print "Usage: python ip2Region.py [ip2region db file] [alrogrithm]"
		print "Algorithm: binary or b-tree"
		return 0

	dbFile    = sys.argv[1]
	method    = 1
	algorithm = "b-tree"
	
	if (not os.path.isfile(dbFile)) or (not os.path.exists(dbFile)):
		print "[Error]: Specified db file is not exists."
		return 0

	if llen > 2:
		algorithm = sys.argv[2]
		if algorithm == "binary":
			method = 2

	print "initializing %s..." % (algorithm)
	print "+----------------------------------+"
	print "| ip2region test script            |"
	print "| Author: komazhang@foxmail.com    |"
	print "| Type 'quit' to exit program      |"
	print "+----------------------------------+"

	searcher = Ip2Region(dbFile);

	while True:
		line = raw_input("ip2region>> ")
		line = line.strip()

		if line == "":
			print "[Error]: Invalid ip address."
			continue

		if line == "quit":
			print "[Info]: Thanks for your use, Bye."
			break

		if not searcher.isip(line):
			print "[Error]: Invalid ip address."
			continue

		if method == 1:
			data = searcher.btreeSearch(line)
		else:
			data = searcher.binarySearch(line)

		if isinstance(data, dict):
			print "[Return]: %s|%s" % (data["city_id"], data["region"])
		else:
			print "[Error]: ", data

	searcher.close()

if __name__ == "__main__":
	testSearch()