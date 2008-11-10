#include "StdAfx.h"
#include "OPPDWrapper.h"

OPPDWrapper::OPPDWrapper(void)
{

	for(int i=0;i<11;i++)
		fileNames[i] = new const char;
	/*fileNames[0] = "M:\\VE_Models\\OPPDUnit\\FIVE_SpreadSheets\\Burning_Duration_Solid.xls";
	fileNames[1] = "M:\\VE_Models\\OPPDUnit\\FIVE_SpreadSheets\\Cable_HRR_Calculations.xls";
	fileNames[2] = "M:\\VE_Models\\OPPDUnit\\FIVE_SpreadSheets\\Detector_Activation_Time.xls";
	fileNames[3] = "M:\\VE_Models\\OPPDUnit\\FIVE_SpreadSheets\\Flame_Height_Calculations.xls";
	fileNames[4] = "M:\\VE_Models\\OPPDUnit\\FIVE_SpreadSheets\\HRR_Flame_Height_Burning_Duration_Calculations.xls";
	fileNames[5] = "M:\\VE_Models\\OPPDUnit\\FIVE_SpreadSheets\\Plume_Temperature_Calculations.xls";
	fileNames[6] = "M:\\VE_Models\\OPPDUnit\\FIVE_SpreadSheets\\Temperature_Closed_Compartment.xls";
	fileNames[7] = "M:\\VE_Models\\OPPDUnit\\FIVE_SpreadSheets\\Temperature_FV1.xls";
	fileNames[8] = "M:\\VE_Models\\OPPDUnit\\FIVE_SpreadSheets\\Temperature_FV2.xls";
	fileNames[9] = "M:\\VE_Models\\OPPDUnit\\FIVE_SpreadSheets\\Temperature_NV.xls";
	fileNames[10] = "M:\\VE_Models\\OPPDUnit\\FIVE_SpreadSheets\\Visibility_Through_Smoke.xls";*/

	fileNames[0] = "C:\\VE_Suite.0.9.3\\units\\FIVE_Spreadsheets\\Burning_Duration_Solid.xls";
	fileNames[1] = "C:\\VE_Suite.0.9.3\\units\\FIVE_Spreadsheets\\Cable_HRR_Calculations.xls";
	fileNames[2] = "C:\\VE_Suite.0.9.3\\units\\FIVE_Spreadsheets\\Detector_Activation_Time.xls";
	fileNames[3] = "C:\\VE_Suite.0.9.3\\units\\FIVE_Spreadsheets\\Flame_Height_Calculations.xls";
	fileNames[4] = "C:\\VE_Suite.0.9.3\\units\\FIVE_Spreadsheets\\HRR_Flame_Height_Burning_Duration_Calculations.xls";
	fileNames[5] = "C:\\VE_Suite.0.9.3\\units\\FIVE_Spreadsheets\\Plume_Temperature_Calculations.xls";
	fileNames[6] = "C:\\VE_Suite.0.9.3\\units\\FIVE_Spreadsheets\\Temperature_Closed_Compartment.xls";
	fileNames[7] = "C:\\VE_Suite.0.9.3\\units\\FIVE_Spreadsheets\\Temperature_FV1.xls";
	fileNames[8] = "C:\\VE_Suite.0.9.3\\units\\FIVE_Spreadsheets\\Temperature_FV2.xls";
	fileNames[9] = "C:\\VE_Suite.0.9.3\\units\\FIVE_Spreadsheets\\Temperature_NV.xls";
	fileNames[10] = "C:\\VE_Suite.0.9.3\\units\\FIVE_Spreadsheets\\Visibility_Through_Smoke.xls";

	/*fileNames[0] = "C:\\TSVEG\\oppd\\OPPDUnit\\FIVE_SpreadSheets\\Burning_Duration_Solid.xls";
	fileNames[1] = "C:\\TSVEG\\oppd\\OPPDUnit\\FIVE_SpreadSheets\\Cable_HRR_Calculations.xls";
	fileNames[2] = "C:\\TSVEG\\oppd\\OPPDUnit\\FIVE_SpreadSheets\\Detector_Activation_Time.xls";
	fileNames[3] = "C:\\TSVEG\\oppd\\OPPDUnit\\FIVE_SpreadSheets\\Flame_Height_Calculations.xls";
	fileNames[4] = "C:\\TSVEG\\oppd\\OPPDUnit\\FIVE_SpreadSheets\\HRR_Flame_Height_Burning_Duration_Calculations.xls";
	fileNames[5] = "C:\\TSVEG\\oppd\\OPPDUnit\\FIVE_SpreadSheets\\Plume_Temperature_Calculations.xls";
	fileNames[6] = "C:\\TSVEG\\oppd\\OPPDUnit\\FIVE_SpreadSheets\\Temperature_Closed_Compartment.xls";
	fileNames[7] = "C:\\TSVEG\\oppd\\OPPDUnit\\FIVE_SpreadSheets\\Temperature_FV1.xls";
	fileNames[8] = "C:\\TSVEG\\oppd\\OPPDUnit\\FIVE_SpreadSheets\\Temperature_FV2.xls";
	fileNames[9] = "C:\\TSVEG\\oppd\\OPPDUnit\\FIVE_SpreadSheets\\Temperature_NV.xls";
	fileNames[10] = "C:\\TSVEG\\oppd\\OPPDUnit\\FIVE_SpreadSheets\\Visibility_Through_Smoke.xls";*/

	wrtempmethod = 0;
    wrtempcalcmethod = 0;
    wrdetectortype = 0;
    wrflametype = 0;
	wrdetacttemp = 0;

	

}

OPPDWrapper::~OPPDWrapper(void)
{
	for(int i=0;i<11;i++)
	{
		oWorkBook[i].Close(COleVariant((short)FALSE),COleVariant((short)fileNames[i]),COleVariant((short)FALSE));
		oExcel[i].Quit();
		oExcel[i].ReleaseDispatch();
	}
}

void OPPDWrapper::loadExcel(void)
{
	COleVariant covTrue((short)TRUE),
			covFalse((short)FALSE),
			covOptional(DISP_E_PARAMNOTFOUND,VT_ERROR);

	for(int j=0;j<11;j++)
	{
	   if(!oExcel[j].CreateDispatch("Excel.Application"))
       {
			AfxMessageBox("Couldn't start Excel and get an application 0bject");
			return;
       }
		// Show Excel to the user.
	    if(j==4||j==10)
			oExcel[j].put_Visible(TRUE);

		oExcel[j].put_UserControl(TRUE);	

		lpDisp[j] = oExcel[j].get_Workbooks();
		ASSERT(lpDisp[j]);
		oWorkBooks[j].AttachDispatch( lpDisp[j] );

		// Open a workbook.
		lpDisp[j] = oWorkBooks[j].Open(fileNames[j],covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional,covOptional); // Excel 2000 requires only 13 arguments
		ASSERT(lpDisp[j]);  // It should have worked.

		oWorkBook[j].AttachDispatch( lpDisp[j] );
		lpDisp[j] = oWorkBook[j].get_Sheets();

		oWorkSheets[j].AttachDispatch( lpDisp[j] );
		
		
	}

}

void OPPDWrapper::killExcel(void)
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

void OPPDWrapper::updateSheet(int tempmethod, int tempcalcmethod, int detectortype, int flametype)
{
	COleVariant covTrue((short)TRUE),
		covFalse((short)FALSE),
		covOptional(DISP_E_PARAMNOTFOUND,VT_ERROR);

	wrtempmethod = tempmethod;
    wrtempcalcmethod = tempcalcmethod;
    wrdetectortype = detectortype;
    wrflametype = flametype;

	for(int j=0;j<11;j++)
	{
		if (j==0||j==1||j==4||j==5||j==10)
			lpDisp[j] = oWorkSheets[j].get_Item( COleVariant((short)1));
		else if (j==2)
			if (detectortype == 1)
				lpDisp[j] = oWorkSheets[j].get_Item( COleVariant((short)2));
			else if (detectortype == 2)
				lpDisp[j] = oWorkSheets[j].get_Item( COleVariant((short)3));
			else
				lpDisp[j] = oWorkSheets[j].get_Item( COleVariant((short)1));
		else if (j==3)
			if (flametype == 1)
				lpDisp[j] = oWorkSheets[j].get_Item( COleVariant((short)2));
			else if (flametype == 2)
				lpDisp[j] = oWorkSheets[j].get_Item( COleVariant((short)3));
			else
				lpDisp[j] = oWorkSheets[j].get_Item( COleVariant((short)1));
		else if (j==6)
			lpDisp[j] = oWorkSheets[j].get_Item( COleVariant((short)2));
		else if (j==7||j==8)
			if (tempmethod == 2)
				lpDisp[j] = oWorkSheets[j].get_Item( COleVariant((short)3));
			else 
				lpDisp[j] = oWorkSheets[j].get_Item( COleVariant((short)2));
		else if (j==9)
			if (tempmethod == 4)
				lpDisp[j] = oWorkSheets[j].get_Item( COleVariant((short)1));
			else 
				lpDisp[j] = oWorkSheets[j].get_Item( COleVariant((short)2));

		oWorkSheet[j].AttachDispatch( lpDisp[j] );
	}
}

void OPPDWrapper::updateFuel(vector<double> fuelpardbls, int fuelselindex)
{
	double dummy = fuelpardbls.at( 0 );
//THE FLAME_HEIGHT_CALCULATIONS WORKSHEET
	lpDisp[3] = oWorkSheet[3].get_Range(COleVariant("F10"),COleVariant("F10"));
	ASSERT(lpDisp[3]);
	oRange[3].AttachDispatch(lpDisp[3]);
	//oRange[3].put_Value(COleVariant(),COleVariant((double)fuelpardbls[0]));
	oRange[3].put_Value(COleVariant(),COleVariant((double)dummy));
	lpDisp[3] = oWorkSheet[3].get_Range(COleVariant("F11"),COleVariant("F11"));
	ASSERT(lpDisp[3]);
	oRange[3].AttachDispatch(lpDisp[3]);
	oRange[3].put_Value(COleVariant(),COleVariant((double)fuelpardbls[1]));

//THE HRR_Flame_Height_Burning_Duration_Calculations WORKSHEET
	lpDisp[4] = oWorkSheet[4].get_Range(COleVariant("F12"),COleVariant("F12"));
	ASSERT(lpDisp[4]);
	oRange[4].AttachDispatch(lpDisp[4]);
	oRange[4].put_Value(COleVariant(),COleVariant((double)fuelpardbls[0]));
	lpDisp[4] = oWorkSheet[4].get_Range(COleVariant("F13"),COleVariant("F13"));
	ASSERT(lpDisp[4]);
	oRange[4].AttachDispatch(lpDisp[4]);
	oRange[4].put_Value(COleVariant(),COleVariant((double)fuelpardbls[1]));
	if(fuelselindex != 0)
		setFuelProps(fuelselindex);
	oWorkSheet[4].Calculate();

//THE Plume_Temperature_Calculations WORKSHEET
	lpDisp[5] = oWorkSheet[5].get_Range(COleVariant("F14"),COleVariant("F14"));
	ASSERT(lpDisp[5]);
	oRange[5].AttachDispatch(lpDisp[5]);
	oRange[5].put_Value(COleVariant(),COleVariant((double)fuelpardbls[1]));

	//Now send the Heat Release Rate to the Plume_Temperature_Calculations WORKSHEET
	//from the HRR_Flame_Height_Burning_Duration_Calculations WORKSHEET
	lpDisp[4] = oWorkSheet[4].get_Range(COleVariant("C57"),COleVariant("C57"));
	ASSERT(lpDisp[4]);
	oRange[4].AttachDispatch(lpDisp[4]);
	COleVariant var;
	var = oRange[4].get_Value(COleVariant());
	lpDisp[5] = oWorkSheet[5].get_Range(COleVariant("F12"),COleVariant("F12"));
	oRange[5].AttachDispatch(lpDisp[5]);
	oRange[5].put_Value(COleVariant(),var);
	
	//Now send the Heat Release Rate to the Temperature_Closed_Compartment WORKSHEET
	lpDisp[6] = oWorkSheet[6].get_Range(COleVariant("F55"),COleVariant("F55"));
	oRange[6].AttachDispatch(lpDisp[6]);
	oRange[6].put_Value(COleVariant(),var);

	//Now send the Heat Release Rate to the Temperature_FV1 WORKSHEET
	lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("F55"),COleVariant("F55"));
	oRange[7].AttachDispatch(lpDisp[7]);
	oRange[7].put_Value(COleVariant(),var);

	//Now send the Heat Release Rate to the Temperature_FV2 WORKSHEET
	if (wrtempmethod == 2)
		lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("F56"),COleVariant("F56"));
	else
		lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("F57"),COleVariant("F57"));
	oRange[8].AttachDispatch(lpDisp[8]);
	oRange[8].put_Value(COleVariant(),var);

	//Now send the Heat Release Rate to the Temperature_NV WORKSHEET
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F56"),COleVariant("F56"));
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),var);

	//Now send the Heat Release Rate to the Detector_Activation_Time WORKSHEET
	if (wrdetectortype == 1)
	{
		lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("E11"),COleVariant("E11"));
		oRange[2].AttachDispatch(lpDisp[2]);
		oRange[2].put_Value(COleVariant(),var);
	}
	else
	{
		lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("F10"),COleVariant("F10"));
		oRange[2].AttachDispatch(lpDisp[2]);
		oRange[2].put_Value(COleVariant(),var);
	}
}

void OPPDWrapper::updateComp(vector<double> compardbls, double intlinthick, int matselindex)
{
//THE TEMPERATURE CLOSED WORKSHEET
	lpDisp[6] = oWorkSheet[6].get_Range(COleVariant("F12"),COleVariant("F12"));
	ASSERT(lpDisp[6]);
	oRange[6].AttachDispatch(lpDisp[6]);
	oRange[6].put_Value(COleVariant(),COleVariant((double)compardbls[0]));
	lpDisp[6] = oWorkSheet[6].get_Range(COleVariant("F13"),COleVariant("F13"));
	ASSERT(lpDisp[6]);
	oRange[6].AttachDispatch(lpDisp[6]);
	oRange[6].put_Value(COleVariant(),COleVariant((double)compardbls[1]));
	lpDisp[6] = oWorkSheet[6].get_Range(COleVariant("F14"),COleVariant("F14"));
	ASSERT(lpDisp[6]);
	oRange[6].AttachDispatch(lpDisp[6]);
	oRange[6].put_Value(COleVariant(),COleVariant((double)compardbls[2]));
	lpDisp[6] = oWorkSheet[6].get_Range(COleVariant("F16"),COleVariant("F16"));
	ASSERT(lpDisp[6]);
	oRange[6].AttachDispatch(lpDisp[6]);
	oRange[6].put_Value(COleVariant(),COleVariant((double)intlinthick));

//THE TEMPERATURE FORCED VENTILATION #1 WORKSHEET
	lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("F13"),COleVariant("F13"));
	ASSERT(lpDisp[7]);
	oRange[7].AttachDispatch(lpDisp[7]);
	oRange[7].put_Value(COleVariant(),COleVariant((double)compardbls[0]));
	lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("F14"),COleVariant("F14"));
	ASSERT(lpDisp[7]);
	oRange[7].AttachDispatch(lpDisp[7]);
	oRange[7].put_Value(COleVariant(),COleVariant((double)compardbls[1]));
	lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("F15"),COleVariant("F15"));
	ASSERT(lpDisp[7]);
	oRange[7].AttachDispatch(lpDisp[7]);
	oRange[7].put_Value(COleVariant(),COleVariant((double)compardbls[2]));
	lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("F17"),COleVariant("F17"));
	ASSERT(lpDisp[7]);
	oRange[7].AttachDispatch(lpDisp[7]);
	oRange[7].put_Value(COleVariant(),COleVariant((double)intlinthick));

//THE TEMPERATURE FORCED VENTILATION #2 WORKSHEET
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("F13"),COleVariant("F13"));
	ASSERT(lpDisp[8]);
	oRange[8].AttachDispatch(lpDisp[8]);
	oRange[8].put_Value(COleVariant(),COleVariant((double)compardbls[0]));
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("F14"),COleVariant("F14"));
	ASSERT(lpDisp[8]);
	oRange[8].AttachDispatch(lpDisp[8]);
	oRange[8].put_Value(COleVariant(),COleVariant((double)compardbls[1]));
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("F15"),COleVariant("F15"));
	ASSERT(lpDisp[8]);
	oRange[8].AttachDispatch(lpDisp[8]);
	oRange[8].put_Value(COleVariant(),COleVariant((double)compardbls[2]));
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("F17"),COleVariant("F17"));
	ASSERT(lpDisp[8]);
	oRange[8].AttachDispatch(lpDisp[8]);
	oRange[8].put_Value(COleVariant(),COleVariant((double)intlinthick));

//THE TEMPERATURE NO VENTILATION WORKSHEET
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F13"),COleVariant("F13"));
	ASSERT(lpDisp[9]);
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),COleVariant((double)compardbls[0]));
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F14"),COleVariant("F14"));
	ASSERT(lpDisp[9]);
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),COleVariant((double)compardbls[1]));
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F15"),COleVariant("F15"));
	ASSERT(lpDisp[9]);
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),COleVariant((double)compardbls[2]));
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F20"),COleVariant("F20"));
	ASSERT(lpDisp[9]);
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),COleVariant((double)intlinthick));

//THE VISIBILITY THROUGH SMOKE WORKSHEET
	lpDisp[10] = oWorkSheet[10].get_Range(COleVariant("F11"),COleVariant("F11"));
	ASSERT(lpDisp[10]);
	oRange[10].AttachDispatch(lpDisp[10]);
	oRange[10].put_Value(COleVariant(),COleVariant((double)compardbls[0]));
	lpDisp[10] = oWorkSheet[10].get_Range(COleVariant("F12"),COleVariant("F12"));
	ASSERT(lpDisp[10]);
	oRange[10].AttachDispatch(lpDisp[10]);
	oRange[10].put_Value(COleVariant(),COleVariant((double)compardbls[1]));
	lpDisp[10] = oWorkSheet[10].get_Range(COleVariant("F13"),COleVariant("F13"));
	ASSERT(lpDisp[10]);
	oRange[10].AttachDispatch(lpDisp[10]);
	oRange[10].put_Value(COleVariant(),COleVariant((double)compardbls[2]));

	if(matselindex != 0)
		setMaterialProps(matselindex);
}

void OPPDWrapper::updateVisib(double massfuelburn, double solidfuelarea, int vismatselindex, int durmatselindex, int vispropselindex, int viscombselindex)
{
//THE VISIBILITY_THROUGH_SMOKE WORKSHEET
	lpDisp[10] = oWorkSheet[10].get_Range(COleVariant("F14"),COleVariant("F14"));
	ASSERT(lpDisp[10]);
	oRange[10].AttachDispatch(lpDisp[10]);
	oRange[10].put_Value(COleVariant(),COleVariant((double)massfuelburn));

//THE BURNING_DURATION_SOLID WORKSHEET
	lpDisp[0] = oWorkSheet[0].get_Range(COleVariant("F14"),COleVariant("F14"));
	ASSERT(lpDisp[0]);
	oRange[0].AttachDispatch(lpDisp[0]);
	oRange[0].put_Value(COleVariant(),COleVariant((double)massfuelburn));

	lpDisp[0] = oWorkSheet[0].get_Range(COleVariant("F15"),COleVariant("F15"));
	ASSERT(lpDisp[0]);
	oRange[0].AttachDispatch(lpDisp[0]);
	oRange[0].put_Value(COleVariant(),COleVariant((double)solidfuelarea));

	if(vismatselindex != 0)
		setVisMatProps(vismatselindex);

	if(durmatselindex != 0)
		setDurMatProps(durmatselindex);

	if(vispropselindex != 0)
		setVisProps(vispropselindex);

	if(viscombselindex != 0)
		setVisCombProps(viscombselindex);

}

void OPPDWrapper::updatePlume(double evalabvfire)
{
//THE Plume_Temperature_Calculations WORKSHEET
	lpDisp[5] = oWorkSheet[5].get_Range(COleVariant("F13"),COleVariant("F13"));
	ASSERT(lpDisp[5]);
	oRange[5].AttachDispatch(lpDisp[5]);
	oRange[5].put_Value(COleVariant(),COleVariant((double)evalabvfire));
}

/*void OPPDWrapper::getMaterials()
{
	COleVariant covTrue((short)TRUE),
			covFalse((short)FALSE),
			covOptional(DISP_E_PARAMNOTFOUND,VT_ERROR);
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("B33"),COleVariant("B48"));
	ASSERT(lpDisp[8]);
	oRange[8].AttachDispatch(lpDisp[8]);
	COleSafeArray matarr(oRange[8].get_Value(covOptional));

	long iRows;
	long index[2];
	matarr.GetUBound(1, &iRows);

	for (int rowCounter = 1; rowCounter <= iRows; rowCounter++) 
	{
		index[0] = rowCounter;
		index[1] = 1;
		COleVariant matData;
		matarr.GetElement(index,matData);
		CString tempHold(matData);
		mats[rowCounter-1] = tempHold;
		tempHold.Delete;
	}

}

void OPPDWrapper::getFuels()
{
	COleVariant covTrue((short)TRUE),
			covFalse((short)FALSE),
			covOptional(DISP_E_PARAMNOTFOUND,VT_ERROR);
	lpDisp[4] = oWorkSheet[4].get_Range(COleVariant("B24"),COleVariant("B43"));
	ASSERT(lpDisp[4]);
	oRange[4].AttachDispatch(lpDisp[4]);
	COleSafeArray fuelarr(oRange[4].get_Value(covOptional));

	long iRows;
	long index[2];
	fuelarr.GetUBound(1, &iRows);

	for (int rowCounter = 1; rowCounter <= iRows; rowCounter++) 
	{
		index[0] = rowCounter;
		index[1] = 1;
		COleVariant fuelData;
		fuelarr.GetElement(index,fuelData);
		CString tempHold(fuelData);
		tblfuels[rowCounter-1] = tempHold;
		tempHold.Delete;
	}

}

void OPPDWrapper::getVisMats()
{
	COleVariant covTrue((short)TRUE),
			covFalse((short)FALSE),
			covOptional(DISP_E_PARAMNOTFOUND,VT_ERROR);
	lpDisp[10] = oWorkSheet[10].get_Range(COleVariant("B22"),COleVariant("B45"));
	ASSERT(lpDisp[10]);
	oRange[10].AttachDispatch(lpDisp[10]);
	COleSafeArray vismatarr(oRange[10].get_Value(covOptional));

	long iRows;
	long index[2];
	vismatarr.GetUBound(1, &iRows);

	for (int rowCounter = 1; rowCounter <= iRows; rowCounter++) 
	{
		index[0] = rowCounter;
		index[1] = 1;
		COleVariant vismatData;
		vismatarr.GetElement(index,vismatData);
		CString tempHold(vismatData);
		tblvismats[rowCounter-1] = tempHold;
		tempHold.Delete;
	}

}

void OPPDWrapper::getDurMats()
{
	COleVariant covTrue((short)TRUE),
			covFalse((short)FALSE),
			covOptional(DISP_E_PARAMNOTFOUND,VT_ERROR);
	lpDisp[0] = oWorkSheet[0].get_Range(COleVariant("B21"),COleVariant("B42"));
	ASSERT(lpDisp[0]);
	oRange[0].AttachDispatch(lpDisp[0]);
	COleSafeArray durmatarr(oRange[0].get_Value(covOptional));

	long iRows;
	long index[2];
	durmatarr.GetUBound(1, &iRows);

	for (int rowCounter = 1; rowCounter <= iRows; rowCounter++) 
	{
		index[0] = rowCounter;
		index[1] = 1;
		COleVariant durmatData;
		durmatarr.GetElement(index,durmatData);
		CString tempHold(durmatData);
		tbldurmats[rowCounter-1] = tempHold;
		tempHold.Delete;
	}

}

void OPPDWrapper::getVisProps()
{
	COleVariant covTrue((short)TRUE),
			covFalse((short)FALSE),
			covOptional(DISP_E_PARAMNOTFOUND,VT_ERROR);
	lpDisp[10] = oWorkSheet[10].get_Range(COleVariant("B51"),COleVariant("B53"));
	ASSERT(lpDisp[10]);
	oRange[10].AttachDispatch(lpDisp[10]);
	COleSafeArray visproparr(oRange[10].get_Value(covOptional));

	long iRows;
	long index[2];
	visproparr.GetUBound(1, &iRows);

	for (int rowCounter = 1; rowCounter <= iRows; rowCounter++) 
	{
		index[0] = rowCounter;
		index[1] = 1;
		COleVariant vispropData;
		visproparr.GetElement(index,vispropData);
		CString tempHold(vispropData);
		tblvisprops[rowCounter-1] = tempHold;
		tempHold.Delete;
	}

}

void OPPDWrapper::getVisComb()
{
	COleVariant covTrue((short)TRUE),
			covFalse((short)FALSE),
			covOptional(DISP_E_PARAMNOTFOUND,VT_ERROR);
	lpDisp[10] = oWorkSheet[10].get_Range(COleVariant("B59"),COleVariant("B60"));
	ASSERT(lpDisp[10]);
	oRange[10].AttachDispatch(lpDisp[10]);
	COleSafeArray viscombarr(oRange[10].get_Value(covOptional));

	long iRows;
	long index[2];
	viscombarr.GetUBound(1, &iRows);

	for (int rowCounter = 1; rowCounter <= iRows; rowCounter++) 
	{
		index[0] = rowCounter;
		index[1] = 1;
		COleVariant viscombData;
		viscombarr.GetElement(index,viscombData);
		CString tempHold(viscombData);
		tblviscomb[rowCounter-1] = tempHold;
		tempHold.Delete;
	}

}

void OPPDWrapper::getDetRti()
{
	COleVariant covTrue((short)TRUE),
			covFalse((short)FALSE),
			covOptional(DISP_E_PARAMNOTFOUND,VT_ERROR);
	lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("B23"),COleVariant("B26"));
	ASSERT(lpDisp[2]);
	oRange[2].AttachDispatch(lpDisp[2]);
	COleSafeArray detrtiarr(oRange[2].get_Value(covOptional));

	long iRows;
	long index[2];
	detrtiarr.GetUBound(1, &iRows);

	for (int rowCounter = 1; rowCounter <= iRows; rowCounter++) 
	{
		index[0] = rowCounter;
		index[1] = 1;
		COleVariant detrtiData;
		detrtiarr.GetElement(index,detrtiData);
		CString tempHold(detrtiData);
		tbldetrti[rowCounter-1] = tempHold;
		tempHold.Delete;
	}

}

void OPPDWrapper::getDetTemprat()
{
	COleVariant covTrue((short)TRUE),
			covFalse((short)FALSE),
			covOptional(DISP_E_PARAMNOTFOUND,VT_ERROR);
	lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("B34"),COleVariant("B40"));
	ASSERT(lpDisp[2]);
	oRange[2].AttachDispatch(lpDisp[2]);
	COleSafeArray dettempratarr(oRange[2].get_Value(covOptional));

	long iRows;
	long index[2];
	dettempratarr.GetUBound(1, &iRows);

	for (int rowCounter = 1; rowCounter <= iRows; rowCounter++) 
	{
		index[0] = rowCounter;
		index[1] = 1;
		COleVariant dettempratData;
		dettempratarr.GetElement(index,dettempratData);
		CString tempHold(dettempratData);
		tbldettemprat[rowCounter-1] = tempHold;
		tempHold.Delete;
	}

}

void OPPDWrapper::getDetSpace(int detacttemp)
{
	wrdetacttemp = detacttemp;
	COleVariant covTrue((short)TRUE),
			covFalse((short)FALSE),
			covOptional(DISP_E_PARAMNOTFOUND,VT_ERROR);
	COleSafeArray detspacearr;

	if (wrdetectortype == 2)
	{
		if (detacttemp == 0)
		{
			lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("C26"),COleVariant("C33"));
			ASSERT(lpDisp[2]);
			oRange[2].AttachDispatch(lpDisp[2]);
			detspacearr.Attach(oRange[2].get_Value(covOptional));
		}
		else if (detacttemp == 1)
		{
			lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("C37"),COleVariant("C44"));
			ASSERT(lpDisp[2]);
			oRange[2].AttachDispatch(lpDisp[2]);
			detspacearr.Attach(oRange[2].get_Value(covOptional));
		}
		else if (detacttemp == 2)
		{
			lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("C48"),COleVariant("C55"));
			ASSERT(lpDisp[2]);
			oRange[2].AttachDispatch(lpDisp[2]);
			detspacearr.Attach(oRange[2].get_Value(covOptional));
		}
		else if (detacttemp == 3)
		{
			lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("C59"),COleVariant("C64"));
			ASSERT(lpDisp[2]);
			oRange[2].AttachDispatch(lpDisp[2]);
			detspacearr.Attach(oRange[2].get_Value(covOptional));
		}
		else if (detacttemp == 4)
		{
			lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("C68"),COleVariant("C72"));
			ASSERT(lpDisp[2]);
			oRange[2].AttachDispatch(lpDisp[2]);
			detspacearr.Attach(oRange[2].get_Value(covOptional));
		}
		else if (detacttemp == 5)
		{
			lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("C76"),COleVariant("C78"));
			ASSERT(lpDisp[2]);
			oRange[2].AttachDispatch(lpDisp[2]);
			detspacearr.Attach(oRange[2].get_Value(covOptional));
		}
		long iRows;
		long index[2];
		detspacearr.GetUBound(1, &iRows);

		for (int rowCounter = 1; rowCounter <= iRows; rowCounter++) 
		{
			index[0] = rowCounter;
			index[1] = 1;
			COleVariant detspaceData;
			detspacearr.GetElement(index,detspaceData);
			CString tempHold(detspaceData);
			tbldetspace[rowCounter-1] = tempHold;
			tempHold.Delete;
		}
	}

}

void OPPDWrapper::getCables()
{
	COleVariant covTrue((short)TRUE),
			covFalse((short)FALSE),
			covOptional(DISP_E_PARAMNOTFOUND,VT_ERROR);
	lpDisp[1] = oWorkSheet[1].get_Range(COleVariant("B19"),COleVariant("B38"));
	ASSERT(lpDisp[1]);
	oRange[1].AttachDispatch(lpDisp[1]);
	COleSafeArray cablearr(oRange[1].get_Value(covOptional));

	long iRows;
	long index[2];
	cablearr.GetUBound(1, &iRows);

	for (int rowCounter = 1; rowCounter <= iRows; rowCounter++) 
	{
		index[0] = rowCounter;
		index[1] = 1;
		COleVariant cableData;
		cablearr.GetElement(index,cableData);
		CString tempHold(cableData);
		tblcabletype[rowCounter-1] = tempHold;
		tempHold.Delete;
	}

	lpDisp[1] = oWorkSheet[1].get_Range(COleVariant("D19"),COleVariant("D38"));
	ASSERT(lpDisp[1]);
	oRange[1].AttachDispatch(lpDisp[1]);
	cablearr.Attach(oRange[1].get_Value(covOptional));
	cablearr.GetUBound(1, &iRows);

	for (int rowCounter = 1; rowCounter <= iRows; rowCounter++) 
	{
		index[0] = rowCounter;
		index[1] = 1;
		COleVariant cableData;
		cablearr.GetElement(index,cableData);
		CString tempHold(cableData);
		tblcabletypebs[rowCounter-1] = tempHold;
		tempHold.Delete;
	}

	for (int i=0;i<20;i++)
		tblcabletype[i] = tblcabletype[i] + tblcabletypebs[i];
}*/
void OPPDWrapper::setFuelProps(int fuelselindex)
{
	sprintf(fuelrangeD,"D%i",fuelselindex+23);
	sprintf(fuelrangeF,"F%i",fuelselindex+23);
	sprintf(fuelrangeH,"H%i",fuelselindex+23);
	CString fuelrDstr = fuelrangeD;
	CString fuelrFstr = fuelrangeF;
	CString fuelrHstr = fuelrangeH;

//GETTING AND DISTRIBUTING THE MASS BURNING RATE OF FUEL BASED ON USER SELECTION
	//HRR_Flame_Height_Burning_Duration_Calculations(Table Values are taken from this sheet and distributed to all sheets needing the selection
	lpDisp[4] = oWorkSheet[4].get_Range(COleVariant((CString)fuelrDstr),COleVariant((CString)fuelrDstr));
	ASSERT(lpDisp[4]);
	oRange[4].AttachDispatch(lpDisp[4]);
	COleVariant var;
	var = oRange[4].get_Value(COleVariant());
	lpDisp[4] = oWorkSheet[4].get_Range(COleVariant("F14"),COleVariant("F14"));
	oRange[4].AttachDispatch(lpDisp[4]);
	oRange[4].put_Value(COleVariant(),var);
	//Flame_Height_Calculations
	lpDisp[3] = oWorkSheet[3].get_Range(COleVariant("F12"),COleVariant("F12"));
	oRange[3].AttachDispatch(lpDisp[3]);
	oRange[3].put_Value(COleVariant(),var);

//GETTING AND DISTRIBUTING THE HEAT OF COMBUSTION OF FUEL
	//HRR_Flame_Height_Burning_Duration_Calculations
	lpDisp[4] = oWorkSheet[4].get_Range(COleVariant((CString)fuelrFstr),COleVariant((CString)fuelrFstr));
	ASSERT(lpDisp[4]);
	oRange[4].AttachDispatch(lpDisp[4]);
	var = oRange[4].get_Value(COleVariant());
	lpDisp[4] = oWorkSheet[4].get_Range(COleVariant("F15"),COleVariant("F15"));
	oRange[4].AttachDispatch(lpDisp[4]);
	oRange[4].put_Value(COleVariant(),var);
	//Flame_Height_Calculations
	lpDisp[3] = oWorkSheet[3].get_Range(COleVariant("F13"),COleVariant("F13"));
	oRange[3].AttachDispatch(lpDisp[3]);
	oRange[3].put_Value(COleVariant(),var);

//GETTING AND DISTRIBUTING THE DENSITY OF FUEL
	//HRR_Flame_Height_Burning_Duration_Calculations
	lpDisp[4] = oWorkSheet[4].get_Range(COleVariant((CString)fuelrHstr),COleVariant((CString)fuelrHstr));
	ASSERT(lpDisp[4]);
	oRange[4].AttachDispatch(lpDisp[4]);
	var = oRange[4].get_Value(COleVariant());
	lpDisp[4] = oWorkSheet[4].get_Range(COleVariant("F16"),COleVariant("F16"));
	oRange[4].AttachDispatch(lpDisp[4]);
	oRange[4].put_Value(COleVariant(),var);
}


void OPPDWrapper::setMaterialProps(int matselindex)
{
	sprintf(matrangeD,"D%i",matselindex+32);
	sprintf(matrangeE,"E%i",matselindex+32);
	sprintf(matrangeF,"F%i",matselindex+32);
	sprintf(matrangeG,"G%i",matselindex+32);
	CString matrDstr = matrangeD;
	CString matrEstr = matrangeE;
	CString matrFstr = matrangeF;
	CString matrGstr = matrangeG;

//GETTING AND DISTRIBUTING THE INTERIOR LINING THERMAL INERTIA BASED ON USER SELECTION
	//Temperature_FV2(Table Values are taken from this sheet and distributed to all sheets needing the selection
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant((CString)matrDstr),COleVariant((CString)matrDstr));
	ASSERT(lpDisp[8]);
	oRange[8].AttachDispatch(lpDisp[8]);
	COleVariant var;
	var = oRange[8].get_Value(COleVariant());
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("F25"),COleVariant("F25"));
	oRange[8].AttachDispatch(lpDisp[8]);
	oRange[8].put_Value(COleVariant(),var);
	//Temperature_Closed_Compartment
	lpDisp[6] = oWorkSheet[6].get_Range(COleVariant("F27"),COleVariant("F27"));
	oRange[6].AttachDispatch(lpDisp[6]);
	oRange[6].put_Value(COleVariant(),var);
	//Temperature_FV1
	lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("F25"),COleVariant("F25"));
	oRange[7].AttachDispatch(lpDisp[7]);
	oRange[7].put_Value(COleVariant(),var);
	//Temperature_NV
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F28"),COleVariant("F28"));
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),var);

//GETTING AND DISTRIBUTING THE INTERIOR LINING THERMAL CONDUCTIVITY
	//Temperature_FV2
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant((CString)matrEstr),COleVariant((CString)matrEstr));
	ASSERT(lpDisp[8]);
	oRange[8].AttachDispatch(lpDisp[8]);
	var = oRange[8].get_Value(COleVariant());
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("F26"),COleVariant("F26"));
	oRange[8].AttachDispatch(lpDisp[8]);
	oRange[8].put_Value(COleVariant(),var);
	//Temperature_Closed_Compartment
	lpDisp[6] = oWorkSheet[6].get_Range(COleVariant("F28"),COleVariant("F28"));
	oRange[6].AttachDispatch(lpDisp[6]);
	oRange[6].put_Value(COleVariant(),var);
	//Temperature_FV1
	lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("F26"),COleVariant("F26"));
	oRange[7].AttachDispatch(lpDisp[7]);
	oRange[7].put_Value(COleVariant(),var);
	//Temperature_NV
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F29"),COleVariant("F29"));
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),var);

//GETTING AND DISTRIBUTING THE INTERIOR LINING SPECIFIC HEAT
	//Temperature_FV2
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant((CString)matrFstr),COleVariant((CString)matrFstr));
	ASSERT(lpDisp[8]);
	oRange[8].AttachDispatch(lpDisp[8]);
	var = oRange[8].get_Value(COleVariant());
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("F27"),COleVariant("F27"));
	oRange[8].AttachDispatch(lpDisp[8]);
	oRange[8].put_Value(COleVariant(),var);
	//Temperature_Closed_Compartment
	lpDisp[6] = oWorkSheet[6].get_Range(COleVariant("F29"),COleVariant("F29"));
	oRange[6].AttachDispatch(lpDisp[6]);
	oRange[6].put_Value(COleVariant(),var);
	//Temperature_FV1
	lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("F27"),COleVariant("F27"));
	oRange[7].AttachDispatch(lpDisp[7]);
	oRange[7].put_Value(COleVariant(),var);
	//Temperature_NV
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F30"),COleVariant("F30"));
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),var);

//GETTING AND DISTRIBUTING THE INTERIOR LINING DENSITY
	//Temperature_FV2
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant((CString)matrGstr),COleVariant((CString)matrGstr));
	ASSERT(lpDisp[8]);
	oRange[8].AttachDispatch(lpDisp[8]);
	var = oRange[8].get_Value(COleVariant());
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("F28"),COleVariant("F28"));
	oRange[8].AttachDispatch(lpDisp[8]);
	oRange[8].put_Value(COleVariant(),var);
	//Temperature_Closed_Compartment
	lpDisp[6] = oWorkSheet[6].get_Range(COleVariant("F30"),COleVariant("F30"));
	oRange[6].AttachDispatch(lpDisp[6]);
	oRange[6].put_Value(COleVariant(),var);
	//Temperature_FV1
	lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("F28"),COleVariant("F28"));
	oRange[7].AttachDispatch(lpDisp[7]);
	oRange[7].put_Value(COleVariant(),var);
	//Temperature_NV
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F31"),COleVariant("F31"));
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),var);

}

void OPPDWrapper::setVisMatProps(int vismatselindex)
{
	sprintf(vismatrangeD,"D%i",vismatselindex+21);
	CString vismatDstr = vismatrangeD;

//GETTING AND DISTRIBUTING THE PARTICULATE YIELD BASED ON USER SELECTION
	//Visibility_Through_Smoke
	lpDisp[10] = oWorkSheet[10].get_Range(COleVariant((CString)vismatDstr),COleVariant((CString)vismatDstr));
	ASSERT(lpDisp[10]);
	oRange[10].AttachDispatch(lpDisp[10]);
	COleVariant var;
	var = oRange[10].get_Value(COleVariant());
	lpDisp[10] = oWorkSheet[10].get_Range(COleVariant("F15"),COleVariant("F15"));
	oRange[10].AttachDispatch(lpDisp[10]);
	oRange[10].put_Value(COleVariant(),var);
}

void OPPDWrapper::setDurMatProps(int durmatselindex)
{
	sprintf(durmatrangeD,"D%i",durmatselindex+20);
	sprintf(durmatrangeF,"F%i",durmatselindex+20);
	CString durmatDstr = durmatrangeD;
	CString durmatFstr = durmatrangeF;

//GETTING AND DISTRIBUTING THE HHR PER UNIT FLOOR AREA AND HEAT OF COMBUSTION BASED ON USER SELECTION
	//Burning_Duration_Solid
	lpDisp[0] = oWorkSheet[0].get_Range(COleVariant((CString)durmatDstr),COleVariant((CString)durmatDstr));
	ASSERT(lpDisp[0]);
	oRange[0].AttachDispatch(lpDisp[0]);
	COleVariant var;
	var = oRange[0].get_Value(COleVariant());
	lpDisp[0] = oWorkSheet[0].get_Range(COleVariant("F16"),COleVariant("F16"));
	oRange[0].AttachDispatch(lpDisp[0]);
	oRange[0].put_Value(COleVariant(),var);

	lpDisp[0] = oWorkSheet[0].get_Range(COleVariant((CString)durmatFstr),COleVariant((CString)durmatFstr));
	ASSERT(lpDisp[0]);
	oRange[0].AttachDispatch(lpDisp[0]);
	var = oRange[0].get_Value(COleVariant());
	lpDisp[0] = oWorkSheet[0].get_Range(COleVariant("F17"),COleVariant("F17"));
	oRange[0].AttachDispatch(lpDisp[0]);
	oRange[0].put_Value(COleVariant(),var);
}
	
void OPPDWrapper::setVisProps(int vispropselindex)
{
	sprintf(visproprangeD,"D%i",vispropselindex+50);
	CString vispropDstr = visproprangeD;

//GETTING AND DISTRIBUTING THE PARTICULATE YIELD BASED ON USER SELECTION
	//Visibility_Through_Smoke
	lpDisp[10] = oWorkSheet[10].get_Range(COleVariant((CString)vispropDstr),COleVariant((CString)vispropDstr));
	ASSERT(lpDisp[10]);
	oRange[10].AttachDispatch(lpDisp[10]);
	COleVariant var;
	var = oRange[10].get_Value(COleVariant());
	lpDisp[10] = oWorkSheet[10].get_Range(COleVariant("F16"),COleVariant("F16"));
	oRange[10].AttachDispatch(lpDisp[10]);
	oRange[10].put_Value(COleVariant(),var);
}

void OPPDWrapper::setVisCombProps(int viscombselindex)
{
	sprintf(viscombrangeD,"D%i",viscombselindex+58);
	CString viscombDstr = viscombrangeD;

//GETTING AND DISTRIBUTING THE PARTICULATE YIELD BASED ON USER SELECTION
	//Visibility_Through_Smoke
	lpDisp[10] = oWorkSheet[10].get_Range(COleVariant((CString)viscombDstr),COleVariant((CString)viscombDstr));
	ASSERT(lpDisp[10]);
	oRange[10].AttachDispatch(lpDisp[10]);
	COleVariant var;
	var = oRange[10].get_Value(COleVariant());
	lpDisp[10] = oWorkSheet[10].get_Range(COleVariant("F17"),COleVariant("F17"));
	oRange[10].AttachDispatch(lpDisp[10]);
	oRange[10].put_Value(COleVariant(),var);
}

void OPPDWrapper::setDetRtiProps(int detrtiselindex)
{
	sprintf(detrtirangeD,"D%i",detrtiselindex+22);
	CString detrtiDstr = detrtirangeD;

//GETTING AND DISTRIBUTING THE GENERIC RESPONSE TIME INDEX BASED ON USER SELECTION
	//Detector_Activation_Time
	lpDisp[2] = oWorkSheet[2].get_Range(COleVariant((CString)detrtiDstr),COleVariant((CString)detrtiDstr));
	ASSERT(lpDisp[2]);
	oRange[2].AttachDispatch(lpDisp[2]);
	COleVariant var;
	var = oRange[2].get_Value(COleVariant());
	lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("F11"),COleVariant("F11"));
	oRange[2].AttachDispatch(lpDisp[2]);
	oRange[2].put_Value(COleVariant(),var);
}

void OPPDWrapper::setDetTempProps(int dettempratselindex)
{
	sprintf(dettempratrangeE,"E%i",dettempratselindex+33);
	CString dettempDstr = dettempratrangeE;

//GETTING AND DISTRIBUTING THE GENERIC TEMPERATURE RATING BASED ON USER SELECTION
	//Detector_Activation_Time
	lpDisp[2] = oWorkSheet[2].get_Range(COleVariant((CString)dettempDstr),COleVariant((CString)dettempDstr));
	ASSERT(lpDisp[2]);
	oRange[2].AttachDispatch(lpDisp[2]);
	COleVariant var;
	var = oRange[2].get_Value(COleVariant());
	lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("F12"),COleVariant("F12"));
	oRange[2].AttachDispatch(lpDisp[2]);
	oRange[2].put_Value(COleVariant(),var);
}

void OPPDWrapper::setDetSpaceProps(int detspaceselindex)
{
//GETTING AND DISTRIBUTING THE RESPONSE TIME INDEX AND ACTIVATION TEMPERATURE BASED ON USER SELECTION
	//Detector_Activation_Time
	if (wrdetacttemp == 0)
	{
		sprintf(detspacerangeD,"D%i",detspaceselindex+25);
		sprintf(detspacerangeE,"E%i",detspaceselindex+25);
	}
	else if (wrdetacttemp == 1)
	{
		sprintf(detspacerangeD,"D%i",detspaceselindex+36);
		sprintf(detspacerangeE,"E%i",detspaceselindex+36);
	}
	else if (wrdetacttemp == 2)
	{
		sprintf(detspacerangeD,"D%i",detspaceselindex+47);
		sprintf(detspacerangeE,"E%i",detspaceselindex+47);
	}
	else if (wrdetacttemp == 3)
	{
		sprintf(detspacerangeD,"D%i",detspaceselindex+58);
		sprintf(detspacerangeE,"E%i",detspaceselindex+58);
	}
	else if (wrdetacttemp == 4)
	{
		sprintf(detspacerangeD,"D%i",detspaceselindex+67);
		sprintf(detspacerangeE,"E%i",detspaceselindex+67);
	}
	else if (wrdetacttemp == 5)
	{
		sprintf(detspacerangeD,"D%i",detspaceselindex+75);
		sprintf(detspacerangeE,"E%i",detspaceselindex+75);
	}

	CString detspaceDstr = detspacerangeD;
	CString detspaceEstr = detspacerangeE;


	lpDisp[2] = oWorkSheet[2].get_Range(COleVariant((CString)detspaceDstr),COleVariant((CString)detspaceDstr));
	ASSERT(lpDisp[2]);
	oRange[2].AttachDispatch(lpDisp[2]);
	COleVariant var;
	var = oRange[2].get_Value(COleVariant());
	lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("F13"),COleVariant("F13"));
	oRange[2].AttachDispatch(lpDisp[2]);
	oRange[2].put_Value(COleVariant(),var);
	lpDisp[2] = oWorkSheet[2].get_Range(COleVariant((CString)detspaceEstr),COleVariant((CString)detspaceEstr));
	ASSERT(lpDisp[2]);
	oRange[2].AttachDispatch(lpDisp[2]);
	var = oRange[2].get_Value(COleVariant());
	lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("F12"),COleVariant("F12"));
	oRange[2].AttachDispatch(lpDisp[2]);
	oRange[2].put_Value(COleVariant(),var);
}


void OPPDWrapper::setCableProps(int cableselindex)
{
	sprintf(cablerangeD,"D%i",cableselindex+18);
	CString cablerDstr = cablerangeD;

	lpDisp[1] = oWorkSheet[1].get_Range(COleVariant((CString)cablerDstr),COleVariant((CString)cablerDstr));
	ASSERT(lpDisp[1]);
	oRange[1].AttachDispatch(lpDisp[1]);
	COleVariant var;
	var = oRange[1].get_Value(COleVariant());
	lpDisp[1] = oWorkSheet[1].get_Range(COleVariant("F12"),COleVariant("F12"));
	oRange[1].AttachDispatch(lpDisp[1]);
	oRange[1].put_Value(COleVariant(),var);
}
void OPPDWrapper::updateAmbient(vector<double> ambpardbls)
{
//THE TEMPERATURE CLOSED WORKSHEET
	lpDisp[6] = oWorkSheet[6].get_Range(COleVariant("F19"),COleVariant("F19"));
	ASSERT(lpDisp[6]);
	oRange[6].AttachDispatch(lpDisp[6]);
	oRange[6].put_Value(COleVariant(),COleVariant((double)ambpardbls[0]));
	lpDisp[6] = oWorkSheet[6].get_Range(COleVariant("F21"),COleVariant("F21"));
	ASSERT(lpDisp[6]);
	oRange[6].AttachDispatch(lpDisp[6]);
	oRange[6].put_Value(COleVariant(),COleVariant((double)ambpardbls[1]));
	lpDisp[6] = oWorkSheet[6].get_Range(COleVariant("F22"),COleVariant("F22"));
	ASSERT(lpDisp[6]);
	oRange[6].AttachDispatch(lpDisp[6]);
	oRange[6].put_Value(COleVariant(),COleVariant((double)ambpardbls[2]));

//THE TEMPERATURE FORCED VENTILATION #1 WORKSHEET
	lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("F20"),COleVariant("F20"));
	ASSERT(lpDisp[7]);
	oRange[7].AttachDispatch(lpDisp[7]);
	oRange[7].put_Value(COleVariant(),COleVariant((double)ambpardbls[0]));
	lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("F22"),COleVariant("F22"));
	ASSERT(lpDisp[7]);
	oRange[7].AttachDispatch(lpDisp[7]);
	oRange[7].put_Value(COleVariant(),COleVariant((double)ambpardbls[1]));
	lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("F23"),COleVariant("F23"));
	ASSERT(lpDisp[7]);
	oRange[7].AttachDispatch(lpDisp[7]);
	oRange[7].put_Value(COleVariant(),COleVariant((double)ambpardbls[2]));

//THE TEMPERATURE FORCED VENTILATION #2 WORKSHEET
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("F20"),COleVariant("F20"));
	ASSERT(lpDisp[8]);
	oRange[8].AttachDispatch(lpDisp[8]);
	oRange[8].put_Value(COleVariant(),COleVariant((double)ambpardbls[0]));
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("F22"),COleVariant("F22"));
	ASSERT(lpDisp[8]);
	oRange[8].AttachDispatch(lpDisp[8]);
	oRange[8].put_Value(COleVariant(),COleVariant((double)ambpardbls[1]));
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("F23"),COleVariant("F23"));
	ASSERT(lpDisp[8]);
	oRange[8].AttachDispatch(lpDisp[8]);
	oRange[8].put_Value(COleVariant(),COleVariant((double)ambpardbls[2]));

//THE TEMPERATURE NO VENTILATION WORKSHEET
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F23"),COleVariant("F23"));
	ASSERT(lpDisp[9]);
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),COleVariant((double)ambpardbls[0]));
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F25"),COleVariant("F25"));
	ASSERT(lpDisp[9]);
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),COleVariant((double)ambpardbls[1]));
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F26"),COleVariant("F26"));
	ASSERT(lpDisp[9]);
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),COleVariant((double)ambpardbls[2]));

	if (wrdetectortype != 1)
	{
		lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("F15"),COleVariant("F15"));
		oRange[2].AttachDispatch(lpDisp[2]);
		oRange[2].put_Value(COleVariant(),COleVariant((double)ambpardbls[0]));
	}
	
}

void OPPDWrapper::updateVent(vector<double> ventpardbls)
{
//THE TEMPERATURE_NV WORKSHEET
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F17"),COleVariant("F17"));
	ASSERT(lpDisp[9]);
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),COleVariant((double)ventpardbls[0]));
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F18"),COleVariant("F18"));
	ASSERT(lpDisp[9]);
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),COleVariant((double)ventpardbls[1]));
	lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F19"),COleVariant("F19"));
	ASSERT(lpDisp[9]);
	oRange[9].AttachDispatch(lpDisp[9]);
	oRange[9].put_Value(COleVariant(),COleVariant((double)ventpardbls[2]));
	if(wrtempmethod == 4)
	{
		lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("F57"),COleVariant("F57"));
		ASSERT(lpDisp[9]);
		oRange[9].AttachDispatch(lpDisp[9]);
		oRange[9].put_Value(COleVariant(),COleVariant((double)ventpardbls[3]));
	}

//THE TEMPERATURE_CLOSED_COMPARTMENT WORKSHEET
	lpDisp[6] = oWorkSheet[6].get_Range(COleVariant("F56"),COleVariant("F56"));
	ASSERT(lpDisp[6]);
	oRange[6].AttachDispatch(lpDisp[6]);
	oRange[6].put_Value(COleVariant(),COleVariant((double)ventpardbls[3]*60));

//THE TEMPERATURE_FV1 WORKSHEET
	lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("F52"),COleVariant("F52"));
	ASSERT(lpDisp[7]);
	oRange[7].AttachDispatch(lpDisp[7]);
	oRange[7].put_Value(COleVariant(),COleVariant((double)ventpardbls[4]));

//THE TEMPERATURE_NV WORKSHEET
	lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("F53"),COleVariant("F53"));
	ASSERT(lpDisp[8]);
	oRange[8].AttachDispatch(lpDisp[8]);
	oRange[8].put_Value(COleVariant(),COleVariant((double)ventpardbls[4]));
}

void OPPDWrapper::updateDetector(vector<double> detectpardbls, int detrtiselindex, int dettempratselindex, int detspaceselindex)
{
//THE DETECTOR_ACTIVATION_TIME WORKSHEET
	//Working on the sprinkler tab
	if (wrdetectortype == 0)
	{
		lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("F13"),COleVariant("F13"));
		ASSERT(lpDisp[2]);
		oRange[2].AttachDispatch(lpDisp[2]);
		oRange[2].put_Value(COleVariant(),COleVariant((double)detectpardbls[0]));
		lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("F14"),COleVariant("F14"));
		ASSERT(lpDisp[2]);
		oRange[2].AttachDispatch(lpDisp[2]);
		oRange[2].put_Value(COleVariant(),COleVariant((double)detectpardbls[1]));

		if(detrtiselindex != 0)
			setDetRtiProps(detrtiselindex);

		if(dettempratselindex != 0)
			setDetTempProps(dettempratselindex);
	}

	if (wrdetectortype == 1)
	{
		lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("E12"),COleVariant("E12"));
		ASSERT(lpDisp[2]);
		oRange[2].AttachDispatch(lpDisp[2]);
		oRange[2].put_Value(COleVariant(),COleVariant((double)detectpardbls[2]));
		lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("E13"),COleVariant("E13"));
		ASSERT(lpDisp[2]);
		oRange[2].AttachDispatch(lpDisp[2]);
		oRange[2].put_Value(COleVariant(),COleVariant((double)detectpardbls[1]));
	}

	if (wrdetectortype == 2)
	{
		lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("F11"),COleVariant("F11"));
		ASSERT(lpDisp[2]);
		oRange[2].AttachDispatch(lpDisp[2]);
		oRange[2].put_Value(COleVariant(),COleVariant((double)detectpardbls[1]));
		lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("F14"),COleVariant("F14"));
		ASSERT(lpDisp[2]);
		oRange[2].AttachDispatch(lpDisp[2]);
		oRange[2].put_Value(COleVariant(),COleVariant((double)detectpardbls[0]));

		if(detspaceselindex != 0)
			setDetSpaceProps(detspaceselindex);
	}

}

void OPPDWrapper::updateCable(double cblburnareadbl, int cableselindex)
{
	lpDisp[1] = oWorkSheet[1].get_Range(COleVariant("F13"),COleVariant("F13"));
	ASSERT(lpDisp[1]);
	oRange[1].AttachDispatch(lpDisp[1]);
	oRange[1].put_Value(COleVariant(),COleVariant((double)cblburnareadbl));

	if(cableselindex != 0)
		setCableProps(cableselindex);

}

void OPPDWrapper::reCalculate( void )
{
	for(int i=0;i<11;i++)
		oWorkSheet[i].Calculate();
}

void OPPDWrapper::getAnswers(void)
{
	COleVariant var;
	lpDisp[0] = oWorkSheet[0].get_Range(COleVariant("C61"),COleVariant("C61"));
	ASSERT(lpDisp[0]);
	oRange[0].AttachDispatch(lpDisp[0]);
	var = oRange[0].get_Value(COleVariant());
	tsec = V_R8(&var); 
	//tsec = oRange[0].get_Value2();

	lpDisp[0] = oWorkSheet[0].get_Range(COleVariant("E61"),COleVariant("E61"));
	ASSERT(lpDisp[0]);
	oRange[0].AttachDispatch(lpDisp[0]);
	var = oRange[0].get_Value(COleVariant());
	tmin = V_R8(&var); 
	
	lpDisp[1] = oWorkSheet[1].get_Range(COleVariant("C51"),COleVariant("C51"));
	ASSERT(lpDisp[1]);
	oRange[1].AttachDispatch(lpDisp[1]);
	var = oRange[1].get_Value(COleVariant());
	hrrkw = V_R8(&var); 

	lpDisp[1] = oWorkSheet[1].get_Range(COleVariant("E51"),COleVariant("E51"));
	ASSERT(lpDisp[1]);
	oRange[1].AttachDispatch(lpDisp[1]);
	var = oRange[1].get_Value(COleVariant());
	hrrbtu = V_R8(&var); 

	if (wrdetectortype == 0)
	{
		lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("E102"),COleVariant("E102"));
		ASSERT(lpDisp[2]);
		oRange[2].AttachDispatch(lpDisp[2]);
		var = oRange[2].get_Value(COleVariant());
		detsprinktime = V_R8(&var); 
		detsmtime = -1;
		detfthtime = -1;
	}
	else if (wrdetectortype == 1)
	{
		lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("E49"),COleVariant("E49"));
		ASSERT(lpDisp[2]);
		oRange[2].AttachDispatch(lpDisp[2]);
		var = oRange[2].get_Value(COleVariant());
		detsmtime = V_R8(&var); 
		detfthtime = -1;
		detsprinktime = -1;
	}
	else if (wrdetectortype == 2)
	{
		lpDisp[2] = oWorkSheet[2].get_Range(COleVariant("E138"),COleVariant("E138"));
		ASSERT(lpDisp[2]);
		oRange[2].AttachDispatch(lpDisp[2]);
		var = oRange[2].get_Value(COleVariant());
		detfthtime = V_R8(&var); 
		detsprinktime = -1;
		detsmtime = -1;
	}

	if (wrflametype == 0)
	{
		lpDisp[3] = oWorkSheet[3].get_Range(COleVariant("E74"),COleVariant("E74"));
		ASSERT(lpDisp[3]);
		oRange[3].AttachDispatch(lpDisp[3]);
		var = oRange[3].get_Value(COleVariant());
		flwallinehgt = V_R8(&var); 
		flcornerhgt = -1;
		flwallhgt = -1;
	}
	else if (wrflametype == 1)
	{
		lpDisp[3] = oWorkSheet[3].get_Range(COleVariant("E60"),COleVariant("E60"));
		ASSERT(lpDisp[3]);
		oRange[3].AttachDispatch(lpDisp[3]);
		var = oRange[3].get_Value(COleVariant());
		flcornerhgt = V_R8(&var); 
		flwallinehgt = -1;
		flwallhgt = -1;
	}
	else if (wrflametype == 2)
	{
		lpDisp[3] = oWorkSheet[3].get_Range(COleVariant("E73"),COleVariant("E73"));
		ASSERT(lpDisp[3]);
		oRange[3].AttachDispatch(lpDisp[3]);
		var = oRange[3].get_Value(COleVariant());
		flwallhgt = V_R8(&var); 
		flwallinehgt = -1;
		flcornerhgt = -1;
	}

	lpDisp[4] = oWorkSheet[4].get_Range(COleVariant("C57"),COleVariant("C57"));
	ASSERT(lpDisp[4]);
	oRange[4].AttachDispatch(lpDisp[4]);
	var = oRange[4].get_Value(COleVariant());
	hrrhrr = V_R8(&var); 

	lpDisp[4] = oWorkSheet[4].get_Range(COleVariant("E81"),COleVariant("E81"));
	ASSERT(lpDisp[4]);
	oRange[4].AttachDispatch(lpDisp[4]);
	var = oRange[4].get_Value(COleVariant());
	hrrburndur = V_R8(&var); 

	lpDisp[4] = oWorkSheet[4].get_Range(COleVariant("E96"),COleVariant("E96"));
	ASSERT(lpDisp[4]);
	oRange[4].AttachDispatch(lpDisp[4]);
	var = oRange[4].get_Value(COleVariant());
	hrrhgthesk = V_R8(&var); 

	lpDisp[4] = oWorkSheet[4].get_Range(COleVariant("E110"),COleVariant("E110"));
	ASSERT(lpDisp[4]);
	oRange[4].AttachDispatch(lpDisp[4]);
	var = oRange[4].get_Value(COleVariant());
	hrrhgtthom = V_R8(&var); 

	lpDisp[5] = oWorkSheet[5].get_Range(COleVariant("E63"),COleVariant("E63"));
	ASSERT(lpDisp[5]);
	oRange[5].AttachDispatch(lpDisp[5]);
	var = oRange[5].get_Value(COleVariant());
	pltemp = V_R8(&var); 

	lpDisp[6] = oWorkSheet[6].get_Range(COleVariant("E85"),COleVariant("E85"));
	ASSERT(lpDisp[6]);
	oRange[6].AttachDispatch(lpDisp[6]);
	var = oRange[6].get_Value(COleVariant());
	tcltemp = V_R8(&var); 

	COleVariant covTrue((short)TRUE),
			covFalse((short)FALSE),
			covOptional(DISP_E_PARAMNOTFOUND,VT_ERROR);

	//FV1 Worksheet*******************************************************************
	if ( wrtempmethod == 1 && wrtempcalcmethod == 1)
	{
		lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("B98"),COleVariant("B106"));
		ASSERT(lpDisp[7]);
		oRange[7].AttachDispatch(lpDisp[7]);
		COleSafeArray fv1timearr(oRange[7].get_Value(covOptional));

		long iRows;
		long index[2];

		fv1timearr.GetUBound(1, &iRows);

		for (int rowCounter = 1; rowCounter <= iRows; rowCounter++) 
		{
			index[0] = rowCounter;
			index[1] = 1;
			COleVariant fv1timeData;
			fv1timearr.GetElement(index,fv1timeData);
			CString tempHold(fv1timeData);
			fv1thicktime[rowCounter-1] = atof(tempHold);
			tempHold.Delete;
		}
		lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("I98"),COleVariant("I106"));
		ASSERT(lpDisp[7]);
		oRange[7].AttachDispatch(lpDisp[7]);
		COleSafeArray fv1temparr(oRange[7].get_Value(covOptional));

		fv1temparr.GetUBound(1, &iRows);

		for (rowCounter = 1; rowCounter <= iRows; rowCounter++) 
		{
			index[0] = rowCounter;
			index[1] = 1;
			COleVariant fv1tempData;
			fv1temparr.GetElement(index,fv1tempData);
			CString tempHold(fv1tempData);
			fv1thicktemp[rowCounter-1] = atof(tempHold);
			tempHold.Delete;
		}
	}
	else if(wrtempmethod == 2 && wrtempcalcmethod == 1)
	{
		lpDisp[7] = oWorkSheet[7].get_Range(COleVariant("E97"),COleVariant("E97"));
		ASSERT(lpDisp[7]);
		oRange[7].AttachDispatch(lpDisp[7]);
		var = oRange[7].get_Value(COleVariant());
		fv1thintemp = V_R8(&var);
	}

	//FV2 Worksheet*******************************************************************
	if ( wrtempmethod == 1 && wrtempcalcmethod == 0)
	{
		lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("B89"),COleVariant("B97"));
		ASSERT(lpDisp[8]);
		oRange[8].AttachDispatch(lpDisp[8]);
		COleSafeArray fv2timearr(oRange[8].get_Value(covOptional));

		long iRows;
		long index[2];

		fv2timearr.GetUBound(1, &iRows);

		for (int rowCounter = 1; rowCounter <= iRows; rowCounter++) 
		{
			index[0] = rowCounter;
			index[1] = 1;
			COleVariant fv2timeData;
			fv2timearr.GetElement(index,fv2timeData);
			CString tempHold(fv2timeData);
			fv2thicktime[rowCounter-1] = atof(tempHold);
			tempHold.Delete;
		}
		lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("H89"),COleVariant("H97"));
		ASSERT(lpDisp[8]);
		oRange[8].AttachDispatch(lpDisp[8]);
		COleSafeArray fv2temparr(oRange[8].get_Value(covOptional));

		fv2temparr.GetUBound(1, &iRows);

		for (rowCounter = 1; rowCounter <= iRows; rowCounter++) 
		{
			index[0] = rowCounter;
			index[1] = 1;
			COleVariant fv2tempData;
			fv2temparr.GetElement(index,fv2tempData);
			CString tempHold(fv2tempData);
			fv2thicktemp[rowCounter-1] = atof(tempHold);
			tempHold.Delete;
		}
	}
	else if(wrtempmethod == 2 && wrtempcalcmethod == 0)
	{
		lpDisp[8] = oWorkSheet[8].get_Range(COleVariant("E87"),COleVariant("E87"));
		ASSERT(lpDisp[8]);
		oRange[8].AttachDispatch(lpDisp[8]);
		var = oRange[8].get_Value(COleVariant());
		fv2thintemp = V_R8(&var);
	}

	//NV Worksheet*******************************************************************
	if ( wrtempmethod == 3 )
	{
		lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("B101"),COleVariant("B109"));
		ASSERT(lpDisp[9]);
		oRange[9].AttachDispatch(lpDisp[9]);
		COleSafeArray nvtimearr(oRange[9].get_Value(covOptional));

		long iRows;
		long index[2];

		nvtimearr.GetUBound(1, &iRows);

		for (int rowCounter = 1; rowCounter <= iRows; rowCounter++) 
		{
			index[0] = rowCounter;
			index[1] = 1;
			COleVariant nvtimeData;
			nvtimearr.GetElement(index,nvtimeData);
			CString tempHold(nvtimeData);
			nvthicktime[rowCounter-1] = atof(tempHold);
			tempHold.Delete;
		}
		lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("H101"),COleVariant("H109"));
		ASSERT(lpDisp[9]);
		oRange[9].AttachDispatch(lpDisp[9]);
		COleSafeArray nvtemparr(oRange[9].get_Value(covOptional));

		nvtemparr.GetUBound(1, &iRows);

		for (rowCounter = 1; rowCounter <= iRows; rowCounter++) 
		{
			index[0] = rowCounter;
			index[1] = 1;
			COleVariant nvtempData;
			nvtemparr.GetElement(index,nvtempData);
			CString tempHold(nvtempData);
			nvthicktemp[rowCounter-1] = atof(tempHold);
			tempHold.Delete;
		}
	}
	else if(wrtempmethod == 4 )
	{
		lpDisp[9] = oWorkSheet[9].get_Range(COleVariant("E98"),COleVariant("E98"));
		ASSERT(lpDisp[9]);
		oRange[9].AttachDispatch(lpDisp[9]);
		var = oRange[9].get_Value(COleVariant());
		nvthintemp = V_R8(&var);
	}

	lpDisp[10] = oWorkSheet[10].get_Range(COleVariant("C101"),COleVariant("C101"));
	ASSERT(lpDisp[10]);
	oRange[10].AttachDispatch(lpDisp[10]);
	var = oRange[10].get_Value(COleVariant());
	visdist = V_R8(&var); 
}