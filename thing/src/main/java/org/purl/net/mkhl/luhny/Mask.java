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
import com.google.common.collect.Lists;
import com.google.common.collect.Ordering;

import java.util.List;

import static com.google.common.base.CharMatcher.DIGIT;

public class Mask {
    private final static CharMatcher SPACE = CharMatcher.anyOf(" -");

    private final Ordering<Integer> ints = Ordering.natural();
    private final StringBuilder builder = new StringBuilder();
    private final List<Maskable> digits = Lists.newArrayList();

    public void addSpace(char space) {
        digits.add(new Space(space));
    }

    public void addDigit(char digit) {
        digits.add(new Digit(digit));
        int i = Maskable.MAX;
        for (Maskable maskable : Lists.reverse(digits)) {
            if (i == 0)
                break;
            if (maskable.add(digit))
                i--;
        }
    }

    public void addOther(char other) {
        processDigits();
        builder.append(other);
    }

    public void add(char next) {
        if (DIGIT.apply(next))
            addDigit(next);
        else if (SPACE.apply(next))
            addSpace(next);
        else
            addOther(next);
    }

    public String mask() {
        processDigits();
        return builder.reverse().toString();
    }

    private void processDigits() {
        int count = 0;
        for (Maskable maskable : digits) {
            count = ints.max(count, maskable.toCount());
            if (count == 0)
                builder.append(maskable.toChar());
            else {
                builder.append(maskable.toMask());
                count = maskable.countDown(count);
            }
        }
        digits.clear();
    }

    public static String process(String input) {
        Mask mask = new Mask();
        for (int i = input.length() - 1; i >= 0; i--)
            mask.add(input.charAt(i));
        return mask.mask();
    }
}
