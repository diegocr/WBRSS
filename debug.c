/* ***** BEGIN LICENSE BLOCK *****
 * Version: MIT/X11 License
 * 
 * Copyright (c) 2007 Diego Casorran
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


#ifdef DEBUG
# include <proto/exec.h>
# include <stdarg.h>
# include <SDI_compiler.h>

#ifndef RawPutChar
# define RawPutChar(CH)	LP1NR( 0x204, RawPutChar, ULONG, CH, d0,,SysBase)
#endif

VOID KPutC(UBYTE ch)
{
	RawPutChar(ch);
}

VOID KPutStr(CONST_STRPTR string)
{
	UBYTE ch;
	
	while((ch = *string++))
		KPutC( ch );
}

STATIC VOID ASM RawPutC(REG(d0,UBYTE ch))
{
	KPutC(ch); 
}

VOID KPrintF(CONST_STRPTR fmt, ...)
{
	va_list args;
	
	va_start(args,fmt);
	RawDoFmt((STRPTR)fmt, args,(VOID (*)())RawPutC, NULL );
	va_end(args);
}

#endif /* DEBUG */
