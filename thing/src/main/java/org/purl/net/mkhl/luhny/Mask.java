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

import com.google.common.base.CharMatcher;
import com.google.common.base.Supplier;
import com.google.common.collect.Lists;
import com.google.common.collect.Ordering;

import java.util.LinkedList;
import java.util.List;

class Mask implements Supplier<String> {
    private final static Ordering<Integer> INT = Ordering.natural();

    private final StringBuilder builder = new StringBuilder();
    private final LinkedList<Luhny> digits = Lists.newLinkedList();

    private void processDigits() {
        int count = 0;
        for (Luhny luhny : Lists.reverse(digits)) {
            count = INT.max(count, luhny.getCount());
            if (count > 0)
                if (luhny.mask())
                    count--;
            builder.append(luhny.getChar());
        }
        digits.clear();
    }

    private void addSpace(char space) {
        digits.addFirst(new Space(space));
    }

    private void addDigit(char digit) {
        digits.addFirst(new Digit(digit));
        int i = Luhny.MAX;
        for (Luhny luhny : digits) {
            if (i == 0)
                break;
            if (luhny.add(digit))
                i--;
        }
    }

    private void addOther(char other) {
        processDigits();
        builder.append(other);
    }

    private void add(char next) {
        switch (next) {
            case ' ':
            case '-':
                addSpace(next);
                break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                addDigit(next);
                break;
            default:
                addOther(next);
                break;
        }
    }

    public String get() {
        processDigits();
        return builder.reverse().toString();
    }

    public static String process(String input) {
        Mask mask = new Mask();
        for (int i = input.length() - 1; i >= 0; i--)
            mask.add(input.charAt(i));
        return mask.get();
    }
}
