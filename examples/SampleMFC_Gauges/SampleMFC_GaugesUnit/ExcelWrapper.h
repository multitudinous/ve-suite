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

class ExcelWrapper
{
public:
	ExcelWrapper(void);
		
	~ExcelWrapper(void);

	void loadExcel(void);
	void killExcel(void);
	void updateSheet(vector<double>,double,double);
	void getAnswers(void);
	
	
	CApplication oExcel;
	CWorkbook oWorkBook;
	CWorkbooks oWorkBooks;
	CWorksheet oWorkSheet;
	CWorksheets oWorkSheets;
	CRange oRange;
	LPDISPATCH lpDisp;

	const char* fileName;
	double calc1;
	double calc2;
	double calc3;
};
