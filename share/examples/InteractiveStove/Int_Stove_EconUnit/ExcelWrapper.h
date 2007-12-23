#ifndef EXCEL_WRAPPER_H
#define EXCEL_WRAPPER_H
#pragma once
#include "stdafx.h"
#include "CApplication.h"
#include "CRange.h"
#include "CWorkbook.h"
#include "CWorkbooks.h"
#include "CWorksheet.h"
#include "CWorksheets.h"
#include <iostream>
#include <vector>
#include <cstdlib>
#include <cstdio>
#include <string>

using namespace std;

class ExcelWrapper
{
public:
	ExcelWrapper(void);
		
	~ExcelWrapper(void);

	void loadExcel(void);
	void killExcel(void);
	void updateSheet(vector<double>,vector<double>,vector<double>,
							   vector<double>,vector<double>,vector<double>,
							   vector<double>,vector<double>,long);
	void getAnswers(void);
	
	
	CApplication oExcel;
	CWorkbook oWorkBook;
	CWorkbooks oWorkBooks;
	CWorksheet oWorkSheet;
	CWorksheets oWorkSheets;
	CRange oRange;
	LPDISPATCH lpDisp;

	char* fileName;
	double baf_mat_cost;
	double baf_const_cost;
	double tot_stove_cost;
};
#endif
