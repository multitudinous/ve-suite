#pragma once
#include "stdafx.h"
#include "CApplication.h"
#include "CRange.h"
#include "CWorkbook.h"
#include "CWorkbooks.h"
#include "CWorksheet.h"
#include "CWorksheets.h"
#include "afxwin.h"
#include <iostream>
#include <vector>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

using namespace std;

class OPPDWrapper
{
public:
	OPPDWrapper(void);
		
	~OPPDWrapper(void);

	void loadExcel(void);
	void killExcel(void);
	void updateSheet(int,int,int,int);
	void updateFuel(vector<double>,int);
	void updateComp(vector<double>,double,int);
	void updateAmbient(vector<double>);
	void updateVisib(double,double,int,int,int,int);
	void updatePlume(double);
	void updateVent(vector<double>);
	void updateDetector(vector<double>,int,int,int);
	void updateCable(double,int);
	void getMaterials(void);
	void getFuels(void);
	void getVisMats(void);
	void getDurMats(void);
	void getVisProps(void);
	void getVisComb(void);
	void getDetRti(void);
	void getDetTemprat(void);
	void getCables(void);
	void getDetSpace(int);
	void setFuelProps(int);
	void setMaterialProps(int);
	void setVisMatProps(int);
	void setDurMatProps(int);
	void setVisProps(int);
	void setVisCombProps(int);
	void setDetRtiProps(int);
	void setDetTempProps(int);
	void setDetSpaceProps(int);
	void setCableProps(int);
	void reCalculate(void);
	void getAnswers(void);
	
	
	CApplication oExcel[11];
	CWorkbook oWorkBook[11];
	CWorkbooks oWorkBooks[11];
	CWorksheet oWorkSheet[11];
	CWorksheets oWorkSheets[11];
	CRange oRange[11];
	LPDISPATCH lpDisp[11];

	/* ARRAY KEY **********************************************************
		[0]  - Burning_Duration_Solid
		[1]  - Cable_HRR_Calculations
		[2]  - Detector_Activation_Time
		[3]  - Flame_Height_Calculations
		[4]  - HRR_Flame_Height_Burning_Duration_Calculations
		[5]  - Plume_Temperature_Calculations
		[6]  - Temperature_Closed_Compartment
		[7]  - Temperature_FV1
		[8]  - Temperature_FV2
		[9]  - Temperature_NV
		[10] - Visibility_Through_Smoke  */

	const char* fileNames[11];
	char matrangeD[5],matrangeE[5],matrangeF[5],matrangeG[5];
	char fuelrangeD[5],fuelrangeF[5],fuelrangeH[5];
	char vismatrangeD[5],durmatrangeD[5],durmatrangeF[5],visproprangeD[5],viscombrangeD[5];
	char detrtirangeD[5];
	char dettempratrangeE[5];
	char detspacerangeD[5],detspacerangeE[5];
	char cablerangeD[5];
	string mats[16];
	string tblfuels[20];
	string tblvismats[24];
	string tbldurmats[24];
	string tblvisprops[3];
	string tblviscomb[2];
	string tbldetrti[4];
	string tbldettemprat[7];
	string tbldetspace[8];
	string tblcabletype[20];
	string tblcabletypebs[20];
	double fv1thicktime[9];
	double fv1thicktemp[9];
	double fv2thicktime[9];
	double fv2thicktemp[9];
	double nvthicktime[9];
	double nvthicktemp[9];
	int wkshtnum[11];
	int wrtempmethod;
    int wrtempcalcmethod;
    int wrdetectortype;
    int wrflametype; 
	int wrdetacttemp;
	double tsec;
	double tmin;
	double hrrkw;
	double hrrbtu;
	double detsprinktime;
	double detsmtime;
	double detfthtime;
	double flwallinehgt;
	double flcornerhgt;
	double flwallhgt;
	double hrrhrr;
	double hrrburndur;
	double hrrhgthesk;
	double hrrhgtthom;
	double pltemp;
	double tcltemp;
	double visdist;
	double fv1thintemp;
	double fv2thintemp;
	double nvthintemp;
};
