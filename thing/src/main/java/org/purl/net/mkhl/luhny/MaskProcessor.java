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

import com.google.common.base.Function;
import com.google.common.collect.Sets;

import java.util.Set;

public class MaskProcessor implements Function<String, String> {
    private final Set<Character> spaces;

    public MaskProcessor(Character... spaces) {
        this(Sets.newHashSet(spaces));
    }

    public MaskProcessor(Set<Character> spaces) {
        this.spaces = spaces;
    }

    public String apply(String input) {
        Mask mask = new Mask();
        for (int i = input.length(); i > 0; i--) {
            char next = input.charAt(i - 1);
            if (Character.isDigit(next))
                mask.addDigit(next);
            else if (spaces.contains(next))
                mask.addSpace(next);
            else
                mask.addOther(next);
        }
        return mask.mask();
    }
}
