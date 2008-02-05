// --- My Includes --- //
#include "stdafx.h"
#include "ExcelWrap.h"
/*
#include "Excel_2000/CApplication.h"
#include "Excel_2000/CWorkbooks.h"
#include "Excel_2000/CWorkbook.h"
#include "Excel_2000/CWorksheet.h"
#include "Excel_2000/CWorksheets.h"
#include "Excel_2000/CRange.h"
*/
#include "Excel_2003/CApplication.h"
#include "Excel_2003/CWorkbooks.h"
#include "Excel_2003/CWorkbook.h"
#include "Excel_2003/CWorksheet.h"
#include "Excel_2003/CWorksheets.h"
#include "Excel_2003/CRange.h"

// --- C/C++ Libraries --- //
#include <fstream>
#include <io.h>
#include <sys/types.h>
#include <sys/stat.h>

////////////////////////////////////////////////////////////////////////////////
ExcelWrap::ExcelWrap()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ExcelWrap::~ExcelWrap()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ExcelWrap::SetSensorData( CApplication* app )
{
    CWorkbook wBook;
    CWorkbooks wBooks;
    CWorksheet wSheet;
    CWorksheets wSheets;
    COleVariant covOptional( static_cast< long >( DISP_E_PARAMNOTFOUND ), VT_ERROR );

    wBooks = app->get_Workbooks();
    std::string fn( "C:\\TSVEG\\NETL\\HyperLab\\HyperLabUnit\\ExcelFiles\\test.xls" );

    //string backSlash = "\\";
    //fn = path+backSlash+fileList[ 0 ];
    //fn = "D:\\TSVEG\\sdt\\ExcelFiles\\test.xls";

    //Excel 2000
    /*
    wBook = wBooks.Open( fn.c_str(), covOptional, covOptional, covOptional,
        covOptional, covOptional, covOptional, covOptional, covOptional,
        covOptional, covOptional, covOptional, covOptional );
    */

    //Excel 2003
    wBook = wBooks.Open( fn.c_str(), covOptional, covOptional, covOptional,
        covOptional, covOptional, covOptional, covOptional, covOptional, covOptional,
        covOptional, covOptional, covOptional, covOptional, covOptional );

    wSheets = wBook.get_Sheets();
    short sheetNo = 1;
    wSheet = wSheets.get_Item( COleVariant( sheetNo ) );
    wSheet.Activate();

    CRange range;
    range = wSheet.get_Range( COleVariant( "A1" ), COleVariant( "A1" ) );
    CString val;
    val = range.get_Value2();
    //AfxMessageBox( val.GetString() );
    sensorData = val.GetString();
}
////////////////////////////////////////////////////////////////////////////////
std::string ExcelWrap::GetSensorData()
{
    return sensorData;

    AfxMessageBox( sensorData.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
