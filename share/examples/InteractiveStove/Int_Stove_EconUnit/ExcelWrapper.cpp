#include "ExcelWrapper.h"
#include <direct.h>
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

ExcelWrapper::ExcelWrapper(void)
{
	baf_mat_cost = 0;
	baf_const_cost = 0;
	tot_stove_cost = 0;

	fileName = new char [ 256 ];
	
	//fileName = "C:\\TSVEG\\VE_Suite\\examples\\Mult_UnitsSample\\AdiabaticFlameTemp\\AdiabaticFlameTempUnit\\AdiabaticFlameTemp.xls";

   char buffer[_MAX_PATH];

    //get the current working directory
    _getcwd(buffer, _MAX_PATH);
    std::string filenameString = std::string( buffer ) + std::string( "\\Econ.xls" );
    strcpy_s( this->fileName, 255, filenameString.c_str() );
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


void ExcelWrapper::updateSheet(vector<double> baffle1,vector<double> baffle2,vector<double> baffle3,
							   vector<double> baffle4,vector<double> baffle5,vector<double> baffle6,
							   vector<double> baffle7,vector<double> cost_array,long numbaffles)
{

	lpDisp = oWorkSheet.get_Range(COleVariant("B4"),COleVariant("B4"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle1.at(0)));
	lpDisp = oWorkSheet.get_Range(COleVariant("E4"),COleVariant("E4"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle1.at(1)));
	lpDisp = oWorkSheet.get_Range(COleVariant("H4"),COleVariant("H4"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle1.at(2)));
	lpDisp = oWorkSheet.get_Range(COleVariant("K4"),COleVariant("K4"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle1.at(3)));
	lpDisp = oWorkSheet.get_Range(COleVariant("T4"),COleVariant("T4"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle1.at(4)));

	lpDisp = oWorkSheet.get_Range(COleVariant("B5"),COleVariant("B5"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle2.at(0)));
	lpDisp = oWorkSheet.get_Range(COleVariant("E5"),COleVariant("E5"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle2.at(1)));
	lpDisp = oWorkSheet.get_Range(COleVariant("H5"),COleVariant("H5"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle2.at(2)));
	lpDisp = oWorkSheet.get_Range(COleVariant("K5"),COleVariant("K5"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle2.at(3)));
	lpDisp = oWorkSheet.get_Range(COleVariant("T5"),COleVariant("T5"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle2.at(4)));

	lpDisp = oWorkSheet.get_Range(COleVariant("B6"),COleVariant("B6"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle3.at(0)));
	lpDisp = oWorkSheet.get_Range(COleVariant("E6"),COleVariant("E6"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle3.at(1)));
	lpDisp = oWorkSheet.get_Range(COleVariant("H6"),COleVariant("H6"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle3.at(2)));
	lpDisp = oWorkSheet.get_Range(COleVariant("K6"),COleVariant("K6"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle3.at(3)));
	lpDisp = oWorkSheet.get_Range(COleVariant("T6"),COleVariant("T6"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle3.at(4)));

	lpDisp = oWorkSheet.get_Range(COleVariant("B7"),COleVariant("B7"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle4.at(0)));
	lpDisp = oWorkSheet.get_Range(COleVariant("E7"),COleVariant("E7"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle4.at(1)));
	lpDisp = oWorkSheet.get_Range(COleVariant("H7"),COleVariant("H7"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle4.at(2)));
	lpDisp = oWorkSheet.get_Range(COleVariant("K7"),COleVariant("K7"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle4.at(3)));
	lpDisp = oWorkSheet.get_Range(COleVariant("T7"),COleVariant("T7"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle4.at(4)));

	lpDisp = oWorkSheet.get_Range(COleVariant("B8"),COleVariant("B8"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle5.at(0)));
	lpDisp = oWorkSheet.get_Range(COleVariant("E8"),COleVariant("E8"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle5.at(1)));
	lpDisp = oWorkSheet.get_Range(COleVariant("H8"),COleVariant("H8"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle5.at(2)));
	lpDisp = oWorkSheet.get_Range(COleVariant("K8"),COleVariant("K8"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle5.at(3)));
	lpDisp = oWorkSheet.get_Range(COleVariant("T8"),COleVariant("T8"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle5.at(4)));

	lpDisp = oWorkSheet.get_Range(COleVariant("B9"),COleVariant("B9"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle6.at(0)));
	lpDisp = oWorkSheet.get_Range(COleVariant("E9"),COleVariant("E9"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle6.at(1)));
	lpDisp = oWorkSheet.get_Range(COleVariant("H9"),COleVariant("H9"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle6.at(2)));
	lpDisp = oWorkSheet.get_Range(COleVariant("K9"),COleVariant("K9"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle6.at(3)));
	lpDisp = oWorkSheet.get_Range(COleVariant("T9"),COleVariant("T9"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle6.at(4)));

	lpDisp = oWorkSheet.get_Range(COleVariant("B10"),COleVariant("B10"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle7.at(0)));
	lpDisp = oWorkSheet.get_Range(COleVariant("E10"),COleVariant("E10"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle7.at(1)));
	lpDisp = oWorkSheet.get_Range(COleVariant("H10"),COleVariant("H10"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle7.at(2)));
	lpDisp = oWorkSheet.get_Range(COleVariant("K10"),COleVariant("K10"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle7.at(3)));
	lpDisp = oWorkSheet.get_Range(COleVariant("T10"),COleVariant("T10"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)baffle7.at(4)));

	lpDisp = oWorkSheet.get_Range(COleVariant("K28"),COleVariant("K28"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)cost_array.at(0)));
	lpDisp = oWorkSheet.get_Range(COleVariant("K29"),COleVariant("K29"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)cost_array.at(1)));
	lpDisp = oWorkSheet.get_Range(COleVariant("H30"),COleVariant("H30"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)cost_array.at(2)));

	lpDisp = oWorkSheet.get_Range(COleVariant("E24"),COleVariant("E24"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	oRange.put_Value(COleVariant(),COleVariant((double)numbaffles));


}

void ExcelWrapper::getAnswers(void)
{
	COleVariant var;

	lpDisp = oWorkSheet.get_Range(COleVariant("K31"),COleVariant("K31"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	var = oRange.get_Value(COleVariant());
	baf_mat_cost = V_R8(&var);

	lpDisp = oWorkSheet.get_Range(COleVariant("K32"),COleVariant("K32"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	var = oRange.get_Value(COleVariant());
	baf_const_cost = V_R8(&var);

	lpDisp = oWorkSheet.get_Range(COleVariant("K34"),COleVariant("K34"));
	ASSERT(lpDisp);
	oRange.AttachDispatch(lpDisp);
	var = oRange.get_Value(COleVariant());
	tot_stove_cost = V_R8(&var);
}

