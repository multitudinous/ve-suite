#include "cfdCalculator.h"

cfdCalculator::cfdCalculator(float *parameters)
{
      JoulFileName = "OrificeCase1.jou";
	   MeshFileName	= "OrificeCase1.msh";
      DBSFileName   = "OrificeCase1.dbs";
	   FluentDataFileName = "OrificeCase1.dat";
	   FluentCasFileName = "OrificeCase1.cas";
	   FluentDPMCasFileName = "OrificeCase1_dpm.cas";
	   FluentDPMDataFileName = "OrificeCase1_dpm.dat";
	   transferstring= "";


      x1=Start_x;y1=Start_y;x2=End_x;y2=End_y;
	   R_elbow=Radius_elbow;
	   R_pipe=Diameter_pipe/2.0;
	   L1=Inlet_length;
      angle=90;
	   angle_rad=angle/180.0*Pi;
      A=x1+2.0*R_elbow*(1-cos(angle_rad));
      B=y1+L1+2*R_elbow*sin(angle_rad);
	   L2=(x2-A)/sin(angle_rad);
	   L3=y2-B-L2*cos(angle_rad);
      transfer_x=R_elbow*(1-cos(angle_rad));
	   transfer_y=R_elbow*sin(angle_rad);
      Orifice_angle=parameters[0];
      L_orifice=Diameter_pipe;
      R_major=parameters[1]*R_pipe;
      R_minor=(0.8*0.8/parameters[1])*R_pipe;
      L_before_orifice=parameters[3]*Diameter_pipe;
      L_after_orifice=L2-L_before_orifice-L_orifice;          
      this->runSystemCall();

}

void cfdCalculator::makeSystemCallFile()
{
   /*
			rm -f twoelbow?.msh 
			gambit -inp .jou > test1 
			fluent 3d -g < inputfile_test.txt > test2

	*/

   SystemCallFile.open("run_fluent");		      
	SystemCallFile<<"rm -f default_id.* core *.dbs"<<std::endl;
   SystemCallFile<<"rm -f FluentFlowData.*"<<std::endl;
   SystemCallFile<<"rm -f "<<MeshFileName<<" "<<DBSFileName<<" "<<FluentCasFileName<<" "<<FluentDataFileName<<std::endl;
	SystemCallFile<<"rm -f "<<FluentDPMCasFileName<<" "<<FluentDPMDataFileName<<" test*"<<" *.lok"<<std::endl;
	SystemCallFile<<"gambit -inp "<<JoulFileName<<" > /dev/null/"<<std::endl;
	SystemCallFile<<"fluent 3d -g < FluentInputFile.txt >/dev/null/"<<std::endl;

	SystemCallFile.close();

}


void cfdCalculator::makeGambitJournalFile()
{
   WholeJoulFile.open(JoulFileName);
	WholeJoulFile<<"$x1="<<x1<<std::endl;
	WholeJoulFile<<"$y1="<<y1<<std::endl;
	WholeJoulFile<<"$x2="<<x2<<std::endl;
	WholeJoulFile<<"$y2="<<y2<<std::endl;
	WholeJoulFile<<"$R_elbow="<<R_elbow<<std::endl;
	WholeJoulFile<<"$R_pipe="<<R_pipe<<std::endl;
	WholeJoulFile<<"$L1="<<L1<<std::endl;
	WholeJoulFile<<"$angle="<<angle<<std::endl;
	WholeJoulFile<<"$A="<<A<<std::endl;
	WholeJoulFile<<"$B="<<B<<std::endl;
	WholeJoulFile<<"$L2="<<L2<<std::endl;
	WholeJoulFile<<"$L3="<<L3<<std::endl;
   WholeJoulFile<<"$L_before_orifice="<<L_before_orifice<<std::endl;
   WholeJoulFile<<"$L_orifice="<<L_orifice<<std::endl;
   WholeJoulFile<<"$L_after_orifice="<<L_after_orifice<<std::endl;
   WholeJoulFile<<"$R_major="<<R_major<<std::endl;
   WholeJoulFile<<"$R_minor="<<R_minor<<std::endl;
   WholeJoulFile<<"$Orifice_angle="<<Orifice_angle<<std::endl;
   WholeJoulFile<<"$transfer_x="<<transfer_x<<std::endl;
	WholeJoulFile<<"$transfer_y="<<transfer_y<<std::endl;
	
   std::ifstream SecondPartJoulFile;
	SecondPartJoulFile.open("secondpart.jou");

		
	while(!SecondPartJoulFile.eof())
	{
		std::string buffer; 
		std::getline(SecondPartJoulFile, buffer, '\n');
		WholeJoulFile << buffer << std::endl;
	}
	SecondPartJoulFile.close();
      
}



void cfdCalculator::makeFluentJournalFile()
{
   WholeFluentInputFile.open("FluentInputFile.txt");
	WholeFluentInputFile<<"rc "<<MeshFileName<<std::endl;
		
	std::ifstream SecondPartFluentInputFile;
	//SecondPartFluentInputFile.open("secondfluentinput.txt");
   SecondPartFluentInputFile.open("udf1.jou");

	while(!SecondPartFluentInputFile.eof())
	{
		std::string buffer; 
		std::getline(SecondPartFluentInputFile, buffer, '\n');
		WholeFluentInputFile << buffer << std::endl;
			
	}
	SecondPartFluentInputFile.close();		
	WholeFluentInputFile<<"wc "<<FluentCasFileName<<std::endl;
	WholeFluentInputFile<<"/solve/it 100"<<std::endl;
	WholeFluentInputFile<<"wd "<<FluentDataFileName<<std::endl;
		
	/*std::ifstream FourthPartFluentInputFile;
	FourthPartFluentInputFile.open("fourthfluentinput.txt");
	while(!FourthPartFluentInputFile.eof())
		{
				std::string buffer; 
				std::getline(FourthPartFluentInputFile, buffer, '\n');
				WholeFluentInputFile << buffer <<std::endl;
			
		}
	FourthPartFluentInputFile.close();*/

	WholeFluentInputFile<<"exit"<<std::endl;
	WholeFluentInputFile<<"yes"<<std::endl;
	WholeFluentInputFile.close();

}

void cfdCalculator::makeInteractiveFluentJournalFile()
{
      WholeFluentInputFile.open("FluentInputFile.txt");
	   WholeFluentInputFile<<"rc "<<MeshFileName<<std::endl;
		
	   std::ifstream SecondPartFluentInputFile;
	   //SecondPartFluentInputFile.open("secondfluentinput.txt");
      SecondPartFluentInputFile.open("udf1.jou");

	   while(!SecondPartFluentInputFile.eof())
	   {
		   std::string buffer; 
		   std::getline(SecondPartFluentInputFile, buffer, '\n');
		   WholeFluentInputFile << buffer << std::endl;
			
	   }
	   SecondPartFluentInputFile.close();
		
	   WholeFluentInputFile<<"wc "<<FluentCasFileName<<std::endl;
	   WholeFluentInputFile<<"/solve/it 100"<<std::endl;
	   WholeFluentInputFile<<"wd "<<FluentDataFileName<<std::endl;
		
	   /*std::ifstream FourthPartFluentInputFile;
	   FourthPartFluentInputFile.open("fourthfluentinput.txt");
	   while(!FourthPartFluentInputFile.eof())
		{
				std::string buffer; 
				std::getline(FourthPartFluentInputFile, buffer, '\n');
				WholeFluentInputFile << buffer <<std::endl;
			
		}
	   FourthPartFluentInputFile.close();*/

	  // WholeFluentInputFile<<"wc "<<FluentDPMCasFileName<<std::endl;
	   //WholeFluentInputFile<<"wd "<<FluentDPMDataFileName<<std::endl;
      WholeFluentInputFile<<"file/export"<<std::endl;
      WholeFluentInputFile<<"avs"<<std::endl;
      WholeFluentInputFile<<"FluentFlowData.avs"<<std::endl;
      WholeFluentInputFile<<"pressure"<<std::endl;
      WholeFluentInputFile<<"velocity-magnitude"<<std::endl;
      WholeFluentInputFile<<"x-velocity"<<std::endl;
      WholeFluentInputFile<<"y-velocity"<<std::endl;
      WholeFluentInputFile<<"z-velocity"<<std::endl;
      WholeFluentInputFile<<"quit"<<std::endl;
      WholeFluentInputFile<<"quit"<<std::endl;
	   WholeFluentInputFile<<"exit"<<std::endl;
	   WholeFluentInputFile<<"yes"<<std::endl;
	   WholeFluentInputFile.close();
}

void cfdCalculator::runSystemCall()
{
      this->makeSystemCallFile();
      this->makeGambitJournalFile();
      this->makeInteractiveFluentJournalFile();
      system("run_fluent");
}



