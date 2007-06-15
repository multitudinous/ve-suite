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
	airexittemp = 0.0;

	fileName = new char [ 256 ];
	
	//fileName = "C:\\TSVEG\\VE_Suite\\examples\\Mult_UnitsSample\\AverageAirTemp\\AverageAirTempUnit\\AverageAirTemp.xls";

   char buffer[_MAX_PATH];

    //get the current working directory
    _getcwd(buffer, _MAX_PATH);
    std::string filenameString = std::string( buffer ) + std::string( "\\AverageAirTemp.xls" );
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


void ExcelWrapper::updateSheet(double intakediam, double airvel, 
                               double intaketemp, double airinlettemp, double intakelength)
{
	lpDisp = oWorkSheet.get_Range(COleVariant("C2"),COleVariant("C2"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)intakediam));
	lpDisp = oWorkSheet.get_Range(COleVariant("C3"),COleVariant("C3"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)airvel));
	lpDisp = oWorkSheet.get_Range(COleVariant("C4"),COleVariant("C4"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)intaketemp));

	lpDisp = oWorkSheet.get_Range(COleVariant("C5"),COleVariant("C5"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)airinlettemp));

	lpDisp = oWorkSheet.get_Range(COleVariant("C6"),COleVariant("C6"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)intakelength));
}

void ExcelWrapper::getAnswers(void)
{
	COleVariant var;

	lpDisp = oWorkSheet.get_Range(COleVariant("C16"),COleVariant("C16"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	var = oRange.get_Value(COleVariant());
	airexittemp = V_R8(&var);
}

