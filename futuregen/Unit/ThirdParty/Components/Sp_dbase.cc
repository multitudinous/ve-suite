
#include "Sp_dbase.h"

namespace Vision21 {

float fSpeciesFileVersion=(float)2.02;

char *SpecieNames[MAX_NOSP]={"H2","CO","H2O","CO2","O2","N2","CH4"};

CSpecieDatabase::CSpecieDatabase()
{

	m_bDoConstCp=false;		// if true, it can save about 10% of your calc. time
	wNoSpecies=6;
	REFERENCE_T=298.0;		/* janaf reference state temperature */
	REFERENCE_P=100000.0;	/* janaf reference state pressure is .1MPa*/

	Specie[H2].lfTDData[Cp0]=14251.09961;
	Specie[H2].lfTDData[Cp1]=-0.319660008;
	Specie[H2].lfTDData[Cp2]=0.001581;
	Specie[H2].lfTDData[Cp3]=-4.58E-07;
	Specie[H2].lfTDData[Cp4]=4.07E-11;
	Specie[H2].lfTDData[CP_TMIN]=300.0;
	Specie[H2].lfTDData[CP_TMAX]=5000.0;
	Specie[H2].lfTDData[Mu0]=3.97E-06;
	Specie[H2].lfTDData[Mu1]=1.93E-08;
	Specie[H2].lfTDData[Mu2]=-3.89E-12;
	Specie[H2].lfTDData[Mu3]=4.98E-16;
	Specie[H2].lfTDData[MU_TMIN]=300.0;
	Specie[H2].lfTDData[MU_TMAX]=3000.0;
	Specie[H2].lfTDData[K0]=0.082897998;
	Specie[H2].lfTDData[K1]=0.00037419;
	Specie[H2].lfTDData[K2]=-2.49E-08;
	Specie[H2].lfTDData[K3]=3.79E-12;
	Specie[H2].lfTDData[K_TMIN]=300.0;
	Specie[H2].lfTDData[K_TMAX]=3000.0;
	Specie[H2].lfTDData[REF_H]=0.0;
	Specie[H2].lfTDData[REF_S]=64760.0;
	Specie[H2].lfTDData[MW]=2.01594;

	Specie[H2O].lfTDData[Cp0]=1534.069946;
	Specie[H2O].lfTDData[Cp1]=0.886699975;
	Specie[H2O].lfTDData[Cp2]=-9.90E-05;
	Specie[H2O].lfTDData[Cp3]=-1.62E-08;
	Specie[H2O].lfTDData[Cp4]=2.95E-12;
	Specie[H2O].lfTDData[CP_TMIN]=300.0;
	Specie[H2O].lfTDData[CP_TMAX]=5000.0;
	Specie[H2O].lfTDData[Mu0]=-1.95E-06;
	Specie[H2O].lfTDData[Mu1]=3.86E-08;
	Specie[H2O].lfTDData[Mu2]=-2.52E-12;
	Specie[H2O].lfTDData[Mu3]=-9.98E-17;
	Specie[H2O].lfTDData[MU_TMIN]=300.0;
	Specie[H2O].lfTDData[MU_TMAX]=3000.0;
	Specie[H2O].lfTDData[K0]=-0.008336;
	Specie[H2O].lfTDData[K1]=9.92E-05;
	Specie[H2O].lfTDData[K2]=4.29E-08;
	Specie[H2O].lfTDData[K3]=-8.68E-12;
	Specie[H2O].lfTDData[K_TMIN]=300.0;
	Specie[H2O].lfTDData[K_TMAX]=3000.0;
	Specie[H2O].lfTDData[REF_H]=-13423600.0;
	Specie[H2O].lfTDData[REF_S]=10472.0;
	Specie[H2O].lfTDData[MW]=18.01534;

	Specie[CO].lfTDData[Cp0]=890.7509766;
	Specie[CO].lfTDData[Cp1]=0.429998994;
	Specie[CO].lfTDData[Cp2]=-0.00016409;
	Specie[CO].lfTDData[Cp3]=2.89E-08;
	Specie[CO].lfTDData[Cp4]=-1.90E-12;
	Specie[CO].lfTDData[CP_TMIN]=300.0;
	Specie[CO].lfTDData[CP_TMAX]=5000.0;
	Specie[CO].lfTDData[Mu0]=6.81E-06;
	Specie[CO].lfTDData[Mu1]=4.12E-08;
	Specie[CO].lfTDData[Mu2]=-9.15E-12;
	Specie[CO].lfTDData[Mu3]=1.22E-15;
	Specie[CO].lfTDData[MU_TMIN]=300.0;
	Specie[CO].lfTDData[MU_TMAX]=3000.0;
	Specie[CO].lfTDData[K0]=0.00515005;
	Specie[CO].lfTDData[K1]=7.22E-05;
	Specie[CO].lfTDData[K2]=-7.90E-09;
	Specie[CO].lfTDData[K3]=4.02E-13;
	Specie[CO].lfTDData[K_TMIN]=300.0;
	Specie[CO].lfTDData[K_TMAX]=3000.0;
	Specie[CO].lfTDData[REF_H]=-3946000.0;
	Specie[CO].lfTDData[REF_S]=7050.0;
	Specie[CO].lfTDData[MW]=28.01055;

	Specie[CO2].lfTDData[Cp0]=657.4500122;
	Specie[CO2].lfTDData[Cp1]=0.90950799;
	Specie[CO2].lfTDData[Cp2]=-0.000423;
	Specie[CO2].lfTDData[Cp3]=8.79E-08;
	Specie[CO2].lfTDData[Cp4]=-6.66E-12;
	Specie[CO2].lfTDData[CP_TMIN]=300.0;
	Specie[CO2].lfTDData[CP_TMAX]=5000.0;
	Specie[CO2].lfTDData[Mu0]=2.03E-06;
	Specie[CO2].lfTDData[Mu1]=4.81E-08;
	Specie[CO2].lfTDData[Mu2]=-1.14E-11;
	Specie[CO2].lfTDData[Mu3]=1.53E-15;
	Specie[CO2].lfTDData[MU_TMIN]=300.0;
	Specie[CO2].lfTDData[MU_TMAX]=3000.0;
	Specie[CO2].lfTDData[K0]=-0.00935;
	Specie[CO2].lfTDData[K1]=9.62E-05;
	Specie[CO2].lfTDData[K2]=-1.72E-08;
	Specie[CO2].lfTDData[K3]=1.80E-12;
	Specie[CO2].lfTDData[K_TMIN]=300.0;
	Specie[CO2].lfTDData[K_TMAX]=3000.0;
	Specie[CO2].lfTDData[REF_H]=-8943700.0;
	Specie[CO2].lfTDData[REF_S]=4854.100098;
	Specie[CO2].lfTDData[MW]=44.00995;

	Specie[O2].lfTDData[Cp0]=815.5900269;
	Specie[O2].lfTDData[Cp1]=0.40259999;
	Specie[O2].lfTDData[Cp2]=-0.0001702;
	Specie[O2].lfTDData[Cp3]=3.65E-08;
	Specie[O2].lfTDData[Cp4]=-2.89E-12;
	Specie[O2].lfTDData[CP_TMIN]=300.0;
	Specie[O2].lfTDData[CP_TMAX]=5000.0;
	Specie[O2].lfTDData[Mu0]=7.24E-06;
	Specie[O2].lfTDData[Mu1]=4.82E-08;
	Specie[O2].lfTDData[Mu2]=-1.07E-11;
	Specie[O2].lfTDData[Mu3]=1.43E-15;
	Specie[O2].lfTDData[MU_TMIN]=300.0;
	Specie[O2].lfTDData[MU_TMAX]=3000.0;
	Specie[O2].lfTDData[K0]=0.00395171;
	Specie[O2].lfTDData[K1]=7.98E-05;
	Specie[O2].lfTDData[K2]=-1.35E-08;
	Specie[O2].lfTDData[K3]=1.71E-12;
	Specie[O2].lfTDData[K_TMIN]=300.0;
	Specie[O2].lfTDData[K_TMAX]=3000.0;
	Specie[O2].lfTDData[REF_H]=0.0;
	Specie[O2].lfTDData[REF_S]=6405.0;
	Specie[O2].lfTDData[MW]=31.9988;

	Specie[N2].lfTDData[Cp0]=898.440979;
	Specie[N2].lfTDData[Cp1]=0.380580008;
	Specie[N2].lfTDData[Cp2]=-0.000129836;
	Specie[N2].lfTDData[Cp3]=2.02E-08;
	Specie[N2].lfTDData[Cp4]=-1.17E-12;
	Specie[N2].lfTDData[CP_TMIN]=300.0;
	Specie[N2].lfTDData[CP_TMAX]=5000.0;
	Specie[N2].lfTDData[Mu0]=6.56E-06;
	Specie[N2].lfTDData[Mu1]=4.24E-08;
	Specie[N2].lfTDData[Mu2]=-9.05E-12;
	Specie[N2].lfTDData[Mu3]=1.20E-15;
	Specie[N2].lfTDData[MU_TMIN]=300.0;
	Specie[N2].lfTDData[MU_TMAX]=3000.0;
	Specie[N2].lfTDData[K0]=0.00585116;
	Specie[N2].lfTDData[K1]=6.85E-05;
	Specie[N2].lfTDData[K2]=-6.47E-09;
	Specie[N2].lfTDData[K3]=2.23E-13;
	Specie[N2].lfTDData[K_TMIN]=300.0;
	Specie[N2].lfTDData[K_TMAX]=3000.0;
	Specie[N2].lfTDData[REF_H]=0.0;
	Specie[N2].lfTDData[REF_S]=6834.;
	Specie[N2].lfTDData[MW]=28.0134;

	Specie[CH4].lfTDData[Cp0]=532.950012207031;
	Specie[CH4].lfTDData[Cp1]=5.82110023498535;
	Specie[CH4].lfTDData[Cp2]=-0.00227800011634827;
	Specie[CH4].lfTDData[Cp3]=4.10700010888831E-007;
	Specie[CH4].lfTDData[Cp4]=-2.79300003264149E-011;
	Specie[CH4].lfTDData[CP_TMIN]=300.0;
	Specie[CH4].lfTDData[CP_TMAX]=5000.0;
	Specie[CH4].lfTDData[Mu0]=3.58600004801701E-006;
	Specie[CH4].lfTDData[Mu1]=3.07299998780763E-008;
	Specie[CH4].lfTDData[Mu2]=-6.99399999720685E-012;
	Specie[CH4].lfTDData[Mu3]=9.50100022155322E-016;
	Specie[CH4].lfTDData[MU_TMIN]=300.0;
	Specie[CH4].lfTDData[MU_TMAX]=3000.0;
	Specie[CH4].lfTDData[K0]=-0.0284599997103214;
	Specie[CH4].lfTDData[K1]=0.000197300003492273;
	Specie[CH4].lfTDData[K2]=1.76399996831833E-008;
	Specie[CH4].lfTDData[K3]=-7.57399958722926E-012;
	Specie[CH4].lfTDData[K_TMIN]=300.0;
	Specie[CH4].lfTDData[K_TMAX]=3000.0;
	Specie[CH4].lfTDData[REF_H]=-4667000.0;
	Specie[CH4].lfTDData[REF_S]=11599.7001953125;
	Specie[CH4].lfTDData[MW]=16.04303;

}

double CSpecieDatabase::getSpecieCp(double Temp,int GPidx)
{
	if(m_bDoConstCp){
		return(CONST_SPHEAT/Specie[GPidx].lfTDData[MW]);
		}

	if(Temp<Specie[GPidx].lfTDData[CP_TMIN]) Temp=Specie[GPidx].lfTDData[CP_TMIN];
	if(Temp>Specie[GPidx].lfTDData[CP_TMAX]) Temp=Specie[GPidx].lfTDData[CP_TMAX];
	int i=(int)(NO_CPVAL+Cp0-1);
	double SpCpval=Specie[GPidx].lfTDData[i];
	i--;
	for(;i>=(int)Cp0;i--){
		SpCpval=SpCpval*Temp+Specie[GPidx].lfTDData[i];
		}
	return(SpCpval);//joule/kg-K
}

double CSpecieDatabase::getSpecieK(double Temp,int GPidx)
{
	if(Temp<Specie[GPidx].lfTDData[K_TMIN]) Temp=Specie[GPidx].lfTDData[K_TMIN];
	if(Temp>Specie[GPidx].lfTDData[K_TMAX]) Temp=Specie[GPidx].lfTDData[K_TMAX];
	int i=(int)(NO_KVAL+K0-1);
	double SpKval=Specie[GPidx].lfTDData[i];
	i--;
	for(;i>=(int)K0;i--){
		SpKval=SpKval*Temp+Specie[GPidx].lfTDData[i];
		}
	return(SpKval);
}

double CSpecieDatabase::getSpecieMu(double Temp,int GPidx)
{
	if(Temp<Specie[GPidx].lfTDData[MU_TMIN]) Temp=Specie[GPidx].lfTDData[MU_TMIN];
	if(Temp>Specie[GPidx].lfTDData[MU_TMAX]) Temp=Specie[GPidx].lfTDData[MU_TMAX];
	int i=(int)(NO_MUVAL+Mu0-1);
	double SpMuval=Specie[GPidx].lfTDData[i];
	i--;
	for(;i>=(int)Mu0;i--){
		SpMuval=SpMuval*Temp+Specie[GPidx].lfTDData[i];
		}
	return(SpMuval);
}



// Private Functions //////////////////////////////////////////////

CSpecieDatabase::~CSpecieDatabase()
{
}

/*
void CSpecieDatabase::Serialize(CArchive & ar)
{
	int	i;	// actual no. of species loaded
	if (ar.IsStoring())	{
		ar<<wNoSpecies;
		for(i=0;i<wNoSpecies;i++){
			ar<<Specie[i].gasname;
			ar<<Specie[i].ref_info;
			for(int j=0;j<=SpDbTransportVarEND;j++){
				ar<<Specie[i].lfTDData[j];
				}
			}
		ar<<REFERENCE_T;	// janaf reference state temperature 
		ar<<REFERENCE_P;	// janaf reference state pressure is .1MPa
		ar<<m_bDoConstCp;
		}
	else {
		ar>>wNoSpecies;
		for(i=0;i<wNoSpecies;i++){
			ar>>Specie[i].gasname;
			ar>>Specie[i].ref_info;
			for(int j=0;j<=SpDbTransportVarEND;j++){
				ar>>Specie[i].lfTDData[j];
				}
			}
		ar>>REFERENCE_T;	// janaf reference state temperature 
		ar>>REFERENCE_P;	// janaf reference state pressure is .1MPa
		ar>>m_bDoConstCp;
		}
}
*/

} // end namespace Vision21
