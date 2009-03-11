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

/**
 * $Id: startup.c,v 0.1 2006/07/06 23:39:43 diegocr Exp $
 */

#define __NOLIBBASE__
#include <proto/exec.h>
#include <proto/dos.h>
#include <workbench/startup.h>

GLOBAL LONG __main_interface (struct WBStartup * _WBenchMsg);

#ifdef USESTDLIB

extern struct WBStartup * _WBenchMsg;

int main ( void )
{
	return((int)__main_interface(_WBenchMsg));
}

#else /* USESTDLIB */

//asm(".globl __start\njbsr __start\nrts");

/***************************************************************************/

struct Library * SysBase       = NULL;
struct Library * UtilityBase   = NULL;
struct Library * DOSBase       = NULL;
struct Library * IntuitionBase = NULL;
struct Library * IconBase      = NULL;
struct Library * GfxBase       = NULL;
struct Library * DiskfontBase  = NULL;
struct Library * DataTypesBase = NULL;

/***************************************************************************/

LONG _start( void )
{
	struct Task * me;
	LONG rc = RETURN_ERROR;
	struct WBStartup * _WBenchMsg = NULL;
	
	SysBase = *(struct Library **) 4L;
	
	if(!((struct Process *)(me=FindTask(NULL)))->pr_CLI)
	{
		struct MsgPort * mp = &((struct Process *)me)->pr_MsgPort;
		
		WaitPort(mp);
		
		_WBenchMsg = (struct WBStartup *)GetMsg(mp);
	}
	
	if((DOSBase = OpenLibrary("dos.library", 39)))
	{
		if((UtilityBase = OpenLibrary("utility.library", 0)))
		{
			if((IconBase = OpenLibrary("icon.library", 0)))
			{
				if((IntuitionBase = OpenLibrary("intuition.library", 0)))
				{
					if((GfxBase = OpenLibrary("graphics.library", 0)))
					{
						if((DiskfontBase = OpenLibrary("diskfont.library", 0)))
						{
							if((DataTypesBase = OpenLibrary("datatypes.library", 0)))
							{
							
					#ifdef STACK_SIZE
						APTR stk;
						ULONG stkSize;
						
						stkSize = 32 + ((((ULONG)me->tc_SPUpper-(ULONG)me->tc_SPLower+STACK_SIZE) + 31) & ~31);
						
						if((stk = AllocMem( stkSize, MEMF_ANY )))
						{
							struct StackSwapStruct stackswap;
							
							stackswap.stk_Lower   = stk;
							stackswap.stk_Upper   = (ULONG)stk+stkSize;
							stackswap.stk_Pointer = (APTR)stackswap.stk_Upper - 32;
							
							StackSwap(&stackswap);
					#endif
							rc = __main_interface ( _WBenchMsg ) ;
							
					#ifdef STACK_SIZE
							StackSwap(&stackswap);
							FreeMem( stk, stkSize );
						}
						else rc = ERROR_NO_FREE_STORE;
					#endif
					
								CloseLibrary( DataTypesBase );
							}
							
							CloseLibrary( DiskfontBase );
						}
						
						CloseLibrary( GfxBase );
					}
					
					CloseLibrary( IntuitionBase );
				}
				
				CloseLibrary( IconBase );
			}
			
			CloseLibrary( UtilityBase );
		}
		
		CloseLibrary( DOSBase );
	}
	
	if( rc == RETURN_ERROR )
		rc = ERROR_INVALID_RESIDENT_LIBRARY;
	
	if(_WBenchMsg != NULL)
	{
		Forbid();
		
		ReplyMsg((struct Message *)_WBenchMsg);
	}
	
	return(rc);
}

#endif /* USESTDLIB */
