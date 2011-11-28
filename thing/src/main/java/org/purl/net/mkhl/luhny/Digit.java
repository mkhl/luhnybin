/*
 * Copyright (c) 2011 Martin Kühl <purl.org/net/mkhl>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package org.purl.net.mkhl.luhny;

public class Digit implements Maskable {
    private final char digit;
    private int total;
    private int count;
    private int valid;

    public Digit(char digit) {
        this.digit = digit;
    }

    public char toChar() {
        return digit;
    }

    public char toMask() {
        return MASK;
    }

    public int toCount() {
        return valid;
    }

    public int countDown(int count) {
        return count - 1;
    }

    public boolean add(char digit) {
        if (count < MAX) {
            total += twice(convert(digit));
            total %= 10;
            count += 1;
            if (total == 0)
                if (count >= MIN)
                    valid = count;
        }
        return true;
    }

    private int twice(int value) {
        if (count % 2 == 0)
            return value;
        int twice = 2 * value;
        return twice % 10 + twice / 10;
    }

    private static int convert(char digit) {
        return Character.digit(digit, 10);
    }
}
