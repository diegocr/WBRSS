/* ***** BEGIN LICENSE BLOCK *****
 * Version: MIT/X11 License
 * 
 * Copyright (c) 2009 Diego Casorran
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 * 
 * Contributor(s):
 *   Diego Casorran <dcasorran@gmail.com> (Original Author)
 * 
 * ***** END LICENSE BLOCK ***** */

#ifndef WBRSS_XMLSUPPORT_H
#define WBRSS_XMLSUPPORT_H


// This is only needed when linking with -lxml2-minimal
// because LIBXML_SAX1_ENABLED wasn't used...

#ifdef NOXMLMINIMAL
# include <libxml/xmlversion.h>
#else
# include <libxml/xmlversion-minimal.h>
# undef LIBXML_DOCB_ENABLED
# undef LIBXML_SAX1_ENABLED
# define LIBXML_DOCB_ENABLED 1
# define LIBXML_SAX1_ENABLED 1
#endif
#include <libxml/parser.h>
#include <libxml/tree.h>

/**
 * xmlParseMemory:
 * @buffer:  an pointer to a char array
 * @size:  the size of the array
 *
 * parse an XML in-memory block and build a tree.
 * 
 * Returns the resulting document tree
 */

extern xmlDocPtr xmlParseMemory(const char *buffer, int size);



#endif /* WBRSS_XMLSUPPORT_H */
