/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "StdAfx.h"
#include "ExcelWrapper.h"
#include <direct.h>
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

ExcelWrapper::ExcelWrapper(void)
{
	calc1 = 0.0;
	calc2 = 0.0;
	calc3 = 0.0;

   fileName = new char [ 256 ];
	
   char buffer[_MAX_PATH];

    //get the current working directory
    _getcwd(buffer, _MAX_PATH);
    std::string filenameString = std::string( buffer ) + std::string( "\\SampleMFC.xls" );
    strcpy( this->fileName, filenameString.c_str() );
}

ExcelWrapper::~ExcelWrapper(void)
{
		oWorkBook.Close(COleVariant((short)FALSE),COleVariant((short)fileName),COleVariant((short)FALSE));
		oExcel.Quit();
		oExcel.ReleaseDispatch();
}

void ExcelWrapper::loadExcel(void)
{
	COleVariant covTrue((short)TRUE),
			covFalse((short)FALSE),
			covOptional(DISP_E_PARAMNOTFOUND,VT_ERROR);

   if(!oExcel.CreateDispatch("Excel.Application"))
   {
			AfxMessageBox("Couldn't start Excel and get an application 0bject");
			return;
   }
		// Show Excel and give control to the user
		oExcel.put_Visible(TRUE);
		oExcel.put_UserControl(TRUE);	

		lpDisp = oExcel.get_Workbooks();
		ASSERT(lpDisp);
		oWorkBooks.AttachDispatch( lpDisp );

		// Open a workbook.
		lpDisp = oWorkBooks.Open(fileName,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional); // Excel 2000 requires only 13 arguments
		ASSERT(lpDisp);

		oWorkBook.AttachDispatch( lpDisp );
		lpDisp = oWorkBook.get_Sheets();

		oWorkSheets.AttachDispatch( lpDisp );	
		lpDisp = oWorkSheets.get_Item( COleVariant((short)1));

		oWorkSheet.AttachDispatch( lpDisp );
}

void ExcelWrapper::killExcel(void)
{
	/*for(int i=0;i<11;i++)
	{
		oWorkBook[i].Close(COleVariant((short)FALSE),COleVariant((short)fileNames[i]),COleVariant((short)FALSE));
		oExcel[i].Quit();
		oExcel[i].ReleaseDispatch();
		oExcel[i].
		//delete oExcel[i];
	}*/
}


void ExcelWrapper::updateSheet(vector<double> dbllist, double dbl1, double dbl2)
{

	lpDisp = oWorkSheet.get_Range(COleVariant("B4"),COleVariant("B4"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)dbllist.at(0)));
	lpDisp = oWorkSheet.get_Range(COleVariant("B5"),COleVariant("B5"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)dbllist.at(1)));
	lpDisp = oWorkSheet.get_Range(COleVariant("B6"),COleVariant("B6"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)dbllist.at(2)));

	lpDisp = oWorkSheet.get_Range(COleVariant("F4"),COleVariant("F4"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)dbl1));

	lpDisp = oWorkSheet.get_Range(COleVariant("J4"),COleVariant("J4"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)dbl2));
}

void ExcelWrapper::getAnswers(void)
{
	COleVariant var;

	lpDisp = oWorkSheet.get_Range(COleVariant("B13"),COleVariant("B13"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	var = oRange.get_Value(COleVariant());
	calc1 = V_R8(&var);

	lpDisp = oWorkSheet.get_Range(COleVariant("F13"),COleVariant("F13"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	var = oRange.get_Value(COleVariant());
	calc2 = V_R8(&var);

	lpDisp = oWorkSheet.get_Range(COleVariant("J13"),COleVariant("J13"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	var = oRange.get_Value(COleVariant());
	calc3 = V_R8(&var);
}

