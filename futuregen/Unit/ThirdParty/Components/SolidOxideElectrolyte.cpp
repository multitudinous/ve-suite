// SolidOxideElectrolyte.cpp: implementation of the CSolidOxideElectrolyte class.
//
//////////////////////////////////////////////////////////////////////

#include "globals.h"
#include "SolidOxideElectrolyte.h"
#include "math.h"

namespace Vision21 {


//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CSolidOxideElectrolyte::CSolidOxideElectrolyte()
{
	CElectrolyte::m_lfGmMoleElectronsPerGmMoleH2=2.0;
	CElectrolyte::m_lfGmMoleElectronsPerGmMoleO2=4.0;
}

CSolidOxideElectrolyte::~CSolidOxideElectrolyte()
{

}

double CSolidOxideElectrolyte::GetMolarDelHStd(double Ti)
{
	double ret=hSp(H2O,Ti)*G_SpDb.Specie[H2O].lfTDData[MW]-0.5*hSp(O2,Ti)*G_SpDb.Specie[O2].lfTDData[MW]-hSp(H2,Ti)*G_SpDb.Specie[H2].lfTDData[MW];//joule/kg-mole
	return(ret);
}

double CSolidOxideElectrolyte::GetMolarDelSStd(double Ti)
{
	double ret=GetReactionDeltaEntropy(Ti,Ti,G_SpDb.REFERENCE_P,G_SpDb.REFERENCE_P);// per kg-mole of H2! without effects of mixing and at std. conditions!!
	return(ret);
}

double CSolidOxideElectrolyte::GetReactionDeltaEntropy(double TCathode, double TAnode, double AnodeP, double CathodeP)
{
	// this is the delS of the basic reaction, not of the process per se
	double delS=MolarEntropySp(H2O,TAnode,AnodeP)-0.5*MolarEntropySp(O2,TCathode,CathodeP)-MolarEntropySp(H2,TAnode,AnodeP);
	return(delS);//joule/kg-mole-H2-K at T[CUR] -- Note, this does not include the entropy of mixing!!!
}

double CSolidOxideElectrolyte::GetReactionDeltaGibbs(double TCathode, double TAnode, double AnodeP, double CathodeP)
{
	// this is the delS of the basic reaction, not of the process per se
	double MolarDelHStd=hSp(H2O,TAnode)*G_SpDb.Specie[H2O].lfTDData[MW]-0.5*hSp(O2,TCathode)*G_SpDb.Specie[O2].lfTDData[MW]-hSp(H2,TAnode)*G_SpDb.Specie[H2].lfTDData[MW];//joule/kg-mole-H2
	double delS=TAnode*MolarEntropySp(H2O,TAnode,AnodeP)-0.5*TCathode*MolarEntropySp(O2,TCathode,CathodeP)-TAnode*MolarEntropySp(H2,TAnode,AnodeP);//joule/kg-moleH2
	return(MolarDelHStd-delS);//joule/kg-mole-H2 -- Note, this does not include the entropy of mixing!!!
}

double CSolidOxideElectrolyte::GetNodeENernstVoltage(int i)
{
	double AnodeTi=An->GetTemp(i);
	double CathodeTi=Ca->GetTemp(i);
	double AnodeP=An->P[i];
	double CathodeP=Ca->P[i];
	double MolarDelHStd=hSp(H2O,AnodeTi)*G_SpDb.Specie[H2O].lfTDData[MW]-0.5*hSp(O2,CathodeTi)*G_SpDb.Specie[O2].lfTDData[MW]-hSp(H2,AnodeTi)*G_SpDb.Specie[H2].lfTDData[MW];//joule/kg-mole
	double MolarDelSStd=GetReactionDeltaEntropy(CathodeTi,AnodeTi,G_SpDb.REFERENCE_P,G_SpDb.REFERENCE_P);// per kg-mole of H2! without effects of mixing and at std. conditions!!
	double MolarDelGStd=GetReactionDeltaGibbs(CathodeTi,AnodeTi,G_SpDb.REFERENCE_P,G_SpDb.REFERENCE_P);// per kg-mole of H2! without effects of mixing and at std. conditions!!
	MolarDelGStd/=1000.0;//delG per g-mole of H2
	double ENurnst=0.0;
	double AnSpH2=An->Sp[CUR][i][H2]+0.000000001;// assume that it will never be zero....
	double CaSpO2=Ca->Sp[CUR][i][O2];
	double AnSpH2O=An->Sp[CUR][i][H2O]+0.00000001;// assume that it will never be zero....that way ln term won't blow up
	ENurnst=-MolarDelGStd/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
	ENurnst+=Runiv*AnodeTi*log(AnSpH2*AnodeP/G_SpDb.REFERENCE_P)/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
	ENurnst-=Runiv*AnodeTi*log(AnSpH2O*AnodeP/G_SpDb.REFERENCE_P)/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
	ENurnst+=Runiv*CathodeTi*log(sqrt(CaSpO2*CathodeP/G_SpDb.REFERENCE_P))/m_lfGmMoleElectronsPerGmMoleH2/FARADAY;
	return(ENurnst);
}

double CSolidOxideElectrolyte::GetReactantActivity(int i,double AnodeP,double CathodeP)
{
	double AnSpH2=An->Sp[CUR][i][H2]+0.000000001;// assume that it will never be zero....
	double CaSpO2=Ca->Sp[CUR][i][O2];
	double ReactantActivity=AnSpH2*AnodeP/G_SpDb.REFERENCE_P*sqrt(CaSpO2*CathodeP/G_SpDb.REFERENCE_P);
	return(ReactantActivity);
}

double CSolidOxideElectrolyte::GetProductActivity(int i,double AnodeP,double CathodeP)
{
	double AnSpH2O=An->Sp[CUR][i][H2O]+0.00000001;// assume that it will never be zero....that way ln term won't blow up
	double ProductActivity=AnSpH2O*AnodeP/G_SpDb.REFERENCE_P;
	return(ProductActivity);
}

} // end namespace Vision21
