#ifndef GLOBALCONST_H
#define GLOBALCONST_H

#include <math.h>

#ifdef WIN32
#include <float.h>
#define FINITE _finite
#else
#define FINITE finite
#endif

#define REAL double

const REAL R=8.314E+7;//8.3144126E+7; // erg/mol*k, univeral gas constant
extern REAL Rc;

const REAL patom=1013250;//1013250.2738308866; //Dyne/square centimeter, 1 atomasphere
#define CHART_LEN 102
#define VALENCE_LEN 11

#include <string>

using namespace std;

const string periodic_chart_name[CHART_LEN] = {"H", "HE", "LI", "BE", "B", "C", "N", "O", "F", "NE", "NA", "MG", "AL", "SI",
		"P", "S", "CL", "AR", "K", "CA", "SC", "TI", "V", "CR", "MN", "FE", "CO", "NI", 
"CU", "ZN", "GA", "GE", "AS", "SE", "BR", "KR", "RB", "SR", "Y", "ZR", "NB", "MO", "TC", "RU", "RH",
 "PD", "AG", "CD", "IN", "SN", "SB", "TE", "I", "XE", "CS", "BA", "LA", "CE", "PR", "ND", "PM", 
 "SM", "EU", "GD", "TB", "DY", "HO", "ER", "TM", "YB", "LU", "HF", "TA", "W", "RE", "OS", "IR", 
"PT", "AU", "HG", "TL", "PB", "BI", "PO", "AT", "RN", "FR", "RA", "AC", "TH", "PA", "U", "NP", "PU",
"AM", "CM", "BK", "CF", "ES", "FM", "D", "E"};

const REAL periodic_chart_wgt[CHART_LEN]={ 1.00797, 4.00260, 6.93900, 9.01220,
 10.81100, 12.01115, 14.00670,  15.99940,
 18.99840, 20.18300, 22.98980,  24.31200,
 26.98150, 28.08600, 30.97380,  32.06400,
 35.45300, 39.94800, 39.10200,  40.08000,
 44.95600, 47.90000, 50.94200,  51.99600,
 54.93800, 55.84700, 58.93320,  58.71000,
 63.54000, 65.37000, 69.72000,  72.59000,
 74.92160, 78.96000, 79.90090,  83.80000,
 85.47000, 87.62000, 88.90500,  91.22000,
 92.90600, 95.94000, 99.00000,  101.07000,
 102.90500,106.40000,107.87000, 112.40000,
 114.82000,118.69000,121.75000, 127.60000,
 126.90440,131.30000,132.90500, 137.34000,
 138.91000,140.12000,140.90700, 144.24000,
 145.00000,150.35000,151.96000, 157.25000,
 158.92400,162.50000,164.93000, 167.26000,
 168.93400,173.04000,174.99700, 178.49000,
 180.94800,183.85000,186.20000, 190.20000,
 192.20000,195.09000,196.96700, 200.59000,
 204.37000,207.19000,208.98000, 210.00000,
 210.00000,222.00000,223.00000, 226.00000,
 227.00000,232.03800,231.00000, 238.03000,
 237.00000,242.00000,243.00000, 247.00000,
 249.00000,251.00000,254.00000, 253.00000,
 002.01410,5.45E-4 };
	
const string elem_valence[VALENCE_LEN] = {"C", "H", "O", "N", "AR", 
"HE", "P", "F", "CL", "S", "HG" };

const REAL valence[VALENCE_LEN] = {4, 1, -2, 0, 0, 0, 5 , -1, 4, 6, -8};

//#define REKS_BANFF
#endif
