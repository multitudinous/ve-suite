
#include "FCMcontroller.h"
#include "iostream.h"
namespace Vision21 {


CFCMcontroller::CFCMcontroller()
{
	
	m_lfPrintTimeStep=0.01;//seconds b/t printed lines
	m_bDoMCFC=false;  // DAS
	m_pFCModel=NULL;
	
	if(m_bDoMCFC){
	}
	else{
		m_pFCModel=&SolidOxide;  // SolidOxide (type CPlanarSolidOxideFuelCellModel) 
		                         // is a member of CFCMcontroller as is the ptr m_pFCModel
	}
}

/////////////////////////////////////////////////////////////////////////////
CFCMcontroller::~CFCMcontroller()
{}

/////////////////////////////////////////////////////////////////////////////
// CFCMcontroller commands

#define NO_SKIP_GETTIME		0	// number of times we get to skip GetMinTime fuction

void CFCMcontroller::OnAnalyzedynamics(double lfStopTime, REIInput *rei_inp, const char *outfile) 
{
	
	
	FILE *f=fopen(outfile, "w");
	if(f!=NULL){
		
		double lfTime=0.0;
		int cnt=0;
		double lfPrntStep=10.01;
		m_lfPrintTimeStep=0.1;//MIKE 0.0001;
		double lfDelTime=0.0005;//MIKE 0.0000005;
		int nGetTime=NO_SKIP_GETTIME;
		
		double lfVoltageSpec=0.7;

		((CPlanarSolidOxideFuelCellModel*)m_pFCModel)->SetREIInputs(rei_inp);
		m_pFCModel->SetupProblem();
		
		bool bDidCase1=false;
		bool bDidCase2=false;
		
		m_pFCModel->PrintConfig(f);
		fprintf(f,"Time[sec],TimeStep[sec],");	m_pFCModel->PrintData(f,true);
		fprintf(f,"%le,0.0,",lfTime);			m_pFCModel->PrintData(f,false);
		
		bool bDoingFullyQS=false;

		double lfConvValue=10.0;  // convergence value

		while(1){
			//			if(lfTime>310.0){
			//				TRACE0("HERE\n");
			//				}
			//			if(lfTime>500.0 && !bDidCase1){
			//				bDidCase1=true;
			//				m_pFCModel->m_lfCathodeTInput=780.0;
			//				lfTime=10.0;
			//				m_pFCModel->m_lfLoadResistance=lfVoltageSpec/m_pFCModel->Electrolyte->GetTotalCurrent();
			//				lfVoltageSpec=-1.0;
			//				m_pFCModel->m_lfLoadResistance=0.00066666667;
			//				m_pFCModel->bDoQuasiSteadySolidPhase=false;
			//				m_pFCModel->m_bDoQuasiSteadyGasFlow=false;
			//				m_pFCModel->SetupContinuationProblem();
			//				}
			//			if(lfTime>6500.0 && !bDidCase2){
			//				bDidCase2=true;
			//				lfTime=6500.0;
			//				m_pFCModel->m_lfLoadResistance=0.0005454545;
			//				m_pFCModel->SetupContinuationProblem();
			//				}
			
			//			m_pFCModel->UpdateBoundaryValuesViaCurrentSpec(1.0);
			m_pFCModel->UpdateBoundaryValues(lfVoltageSpec,true);
			
			nGetTime++;
			if(nGetTime>NO_SKIP_GETTIME  || lfTime<0.001){
				lfDelTime=m_pFCModel->GetMinTimeStep()*0.5;
				if(lfDelTime<0.0) {
					bDoingFullyQS=true;
				}
				else {
					bDoingFullyQS=false;
					lfDelTime/=5.00;// dynamic analysis is less stable--can get 'spatial-ringing' behavior
				}
				nGetTime=0;
			}
			if(!bDoingFullyQS){
				lfTime+=lfDelTime;
				lfPrntStep+=lfDelTime;
			}
			
			if(m_pFCModel->AnalyzeFuelCell(lfDelTime)==-1) break;  // LOOP EXIT POINT
			cnt++;
			if(lfPrntStep>m_lfPrintTimeStep||cnt>60000 || bDoingFullyQS){// || (lfTime>0.99*lfStopTime&&cnt>100)){
				cnt=0;
				lfPrntStep=0.0;
				if(bDoingFullyQS){
					fprintf(f,"%le,", lfConvValue);
				}
				else {
					fprintf(f,"%le,%le,",lfTime,lfDelTime);	
				}
				m_pFCModel->PrintData(f,false);
				printf("t=%le,Telec=%le,TelecCAve=%le,",lfTime,m_pFCModel->Electrolyte->m_lfTemp[CUR][1],m_pFCModel->Electrolyte->CurrentAveCellTemp());
				printf("Amp=%le,V=%le  ",m_pFCModel->Electrolyte->GetTotalCurrent(),m_pFCModel->Electrolyte->GetLoadVoltage());
				printf("FU%le,",m_pFCModel->AnodeGas->GetUtilization());
				printf("OU%le,",m_pFCModel->CathodeGas->GetUtilization());
				printf("An_mdotIN%le,Ca_mdotIN%le\n",m_pFCModel->AnodeGas->GetMassFlowOut(0),m_pFCModel->CathodeGas->GetMassFlowOut(0));
			}
			
			if(!bDoingFullyQS){   // dynamic calculation option (das)
				//then we are doing a dynamic calculation
				if(lfTime>lfStopTime) break;    // LOOP EXIT POINT
			}
			else {   // QS calculation option (das)
				//then we are doing a fully QS calculation, so test for convergence
				if(lfConvValue<CURRENT_PRECISION*1.1) break;// LOOP EXIT POINT // put this before evaluation so that we know we'll go at least two times through so CUR and NXT varbs will all get updated
				lfConvValue=m_pFCModel->QS_Converged();
			}
		}
		double lfFuelUtil=m_pFCModel->AnodeGas->GetUtilization();//currently just for H2
		if(!m_bDoMCFC){
			m_pFCModel->AnalyzeOverallPerformanceFollowingDetailedAnalysis(f,lfFuelUtil);
		}
	}
	if(f!=NULL)fclose(f);
}

////////////////////////////////////////////////////////////////////////////////////
void CFCMcontroller::OnAnalyzecontinue() 
{
	// TODO: Add your command handler code here
	FILE *f=fopen("Continue.dat","w");
	if(f==NULL) return;
	
	int cnt=0;
	double lfPrntStep=0.0;
	double lfDelTime=0.0000005;
	int nGetTime=NO_SKIP_GETTIME;
	
	double lfStopTime=30000.0;
	m_lfPrintTimeStep=5.0;
	m_pFCModel->m_szDescription="Continuing With same Load!!";
	
	double lfVoltageSpec=0.7;
	double lfTime=1002.0;
	
	m_pFCModel->SetupContinuationProblem();
	
	bool bDidCase1=false;
	bool bDidCase2=false;
	
	m_pFCModel->PrintConfig(f);
	fprintf(f,"Time[sec],TimeStep[sec],");	m_pFCModel->PrintData(f,true);
	fprintf(f,"%le,0.0,",lfTime);			m_pFCModel->PrintData(f,false);
	
	bool bDoingFullyQS=false;
	double lfConvValue=10.0;
	while(1){
		//			if(lfTime>3500.0 && !bDidCase1){
		//				bDidCase1=true;
		//				m_pFCModel->m_lfLoadResistance/=0.9;
		//				m_pFCModel->bDoQuasiSteadySolidPhase=true;
		//				m_pFCModel->m_bDoQuasiSteadyGasFlow=false;
		//				m_pFCModel->SetupContinuationProblem();
		//				}
		//			if(lfTime>12800.0 && !bDidCase2){
		//				bDidCase2=true;
		//				m_pFCModel->m_bDoQuasiSteadyGasFlow=false;
		//				m_pFCModel->m_lfLoadResistance*=1.1;
		//				m_pFCModel->bDoQuasiSteadySolidPhase=false;
		//				m_pFCModel->m_lfLoadResistance*=1.2;
		//				m_pFCModel->DoubleAnodePressDrop_Vel();// use this to change the flow rate...
		//				m_pFCModel->SetupContinuationProblem();
		//				}
		
		//			m_pFCModel->UpdateBoundaryValuesViaCurrentSpec(1.0);
		m_pFCModel->UpdateBoundaryValues(lfVoltageSpec,true);
		nGetTime++;
		if(nGetTime>NO_SKIP_GETTIME || lfTime<0.001){
			lfDelTime=m_pFCModel->GetMinTimeStep()*0.5;// /1.5;//3.0;// if you get oscillations in the nodes, may need to go to smaller time steps!
			if(lfDelTime<0.0) {
				bDoingFullyQS=true;
			}
			else {
				bDoingFullyQS=false;
			}
			nGetTime=0;
		}
		if(!bDoingFullyQS){
			lfTime+=lfDelTime;
			lfPrntStep+=lfDelTime;
		}
		if(m_pFCModel->AnalyzeFuelCell(lfDelTime)==-1) break;
		cnt++;
		if(lfPrntStep>m_lfPrintTimeStep||cnt>60000 || bDoingFullyQS){// || (lfTime>0.99*lfStopTime&&cnt>100)){
			cnt=0;
			lfPrntStep=0.0;
			if(bDoingFullyQS){
				fprintf(f,"%le,", lfConvValue);
				printf("%le\n",lfConvValue);
			}
			else {
				fprintf(f,"%le,%le,",lfTime,lfDelTime);	
			}
			m_pFCModel->PrintData(f,false);
			printf("t=%le,Telec=%le,TelecCAve=%le,",lfTime,m_pFCModel->Electrolyte->m_lfTemp[CUR][1],m_pFCModel->Electrolyte->CurrentAveCellTemp());
			printf("Amp=%le,V=%le  ",m_pFCModel->Electrolyte->GetTotalCurrent(),m_pFCModel->Electrolyte->GetLoadVoltage());
			printf("FU%le,",m_pFCModel->AnodeGas->GetUtilization());
			printf("OU%le,",m_pFCModel->CathodeGas->GetUtilization());
			printf("An_mdotIN%le,Ca_mdotIN%le\n",m_pFCModel->AnodeGas->GetMassFlowOut(0),m_pFCModel->CathodeGas->GetMassFlowOut(0));
		}
		if(!bDoingFullyQS){
			//then we are doing a dynamic calculation
			if(lfTime>lfStopTime) break;
		}
		else {
			// fully QS calculation, or test for convergence
			if(lfConvValue<CURRENT_PRECISION*1.1) break;// put this before evaluation so that we know we'll go at least two times through so CUR and NXT varbs will all get updated
			lfConvValue=m_pFCModel->QS_Converged();
		}
	}
	double lfFuelUtil=m_pFCModel->AnodeGas->GetUtilization();//currently just for H2
	if(!m_bDoMCFC){
		m_pFCModel->AnalyzeOverallPerformanceFollowingDetailedAnalysis(f,lfFuelUtil);
	}
	if(f!=NULL)fclose(f);
}


} // end namespace Vision21
