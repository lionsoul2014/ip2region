// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/07/14

package org.lionsoul.ip2region.xdb;

import java.util.ArrayList;
import java.util.List;

public class Segment {
    public final byte[] startIP;
    public final byte[] endIP;
    public final String region;

    public Segment(final byte[] startIP, final byte[] endIP, String region) {
        this.startIP = startIP;
        this.endIP = endIP;
        this.region = region;
    }

    // split the current segment for vector index
    public List<Segment> split() {
        final int sByte1 = (int) (startIP[0] & 0xFF);
        final int eByte1 = (int) (endIP[0] & 0xFF);
        final List<Segment> tList =  new ArrayList<Segment>();
        for (int i = sByte1; i <= eByte1; i++) {
            final byte[] sip = new byte[startIP.length];
            final byte[] eip = new byte[startIP.length];

            if (i == sByte1) {
                System.arraycopy(startIP, 0, sip, 0, sip.length);
            } else {
                sip[0] = (byte) (i & 0xFF);
            }

            if (i == eByte1) {
                System.arraycopy(endIP, 0, eip, 0, eip.length);
            } else {
                eip[0] = (byte)(i & 0xFF);
                // fill the rest buffer with 0xFF
                for (int j = 1; j < eip.length; j++) {
                    eip[j] = (byte) (0xFF);
                }
            }

            // append the new segment:
            // @Note: Don't bother to copy the region.
            tList.add(new Segment(sip, eip, null));
        }

        // 2, split the segments with the second byte
        final List<Segment> segList = new ArrayList<Segment>();
        for (Segment seg : tList) {
            final int sByte2 = (int) (seg.startIP[1] & 0xFF);
            final int eByte2 = (int) (seg.endIP[1] & 0xFF);
            for (int i = sByte2; i <= eByte2; i++) {
                final byte[] sip = new byte[seg.startIP.length];
                final byte[] eip = new byte[seg.startIP.length];
                sip[0] = seg.startIP[0];
                eip[0] = seg.startIP[0];

                if (i == sByte2) {
                    System.arraycopy(seg.startIP, 0, sip, 0, seg.startIP.length);
                } else {
                    sip[1] = (byte) (i & 0xFF);
                }

                if (i == eByte2) {
                    System.arraycopy(seg.endIP, 0, eip, 0, seg.endIP.length);
                } else {
                    eip[1] = (byte) (i & 0xFF);
                    for (int j = 2; j < seg.endIP.length; j++) {
                        eip[j] = (byte) 0xFF;
                    }
                }

                // append the new segment
                segList.add(new Segment(sip, eip, region));
            }
        }

        return segList;
    }

    @Override public String toString() {
        return Util.ipToString(startIP) + "|" + Util.ipToString(endIP) + "|" + region;
    }

    // check if the an IP address in within this segment
    public boolean contains(final byte[] ip) {
        return Util.ipCompare(ip, startIP) >= 0 && Util.ipCompare(ip, endIP) <= 0;
    }

    // parser the Segment from an input string
    public static Segment parse(String input) throws Exception {
        final String[] ps = input.trim().split("\\|", 3);
        if (ps.length != 3) {
            throw new Exception("invalid ip segment `"+input+"`");
        }

        final byte[] sip = Util.parseIP(ps[0]);
        final byte[] eip = Util.parseIP(ps[1]);
        if (Util.ipCompare(sip, eip) > 0) {
            throw new Exception("start ip `"+ps[0]+"` should not be greater than end ip `"+ps[1]+"`");
        }

        return new Segment(sip, eip, ps[2]);
    }

}
