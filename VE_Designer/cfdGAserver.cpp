#include "cfdGAserver.h"

cfdGAserver::cfdGAserver()
{
   this->changemanager = cfdChangeManager::getInstance();
   this->getInfoFromChangeManager();
    
}

cfdGAserver::~cfdGAserver()
{
   //this->_GAserversubject->deleteObserver(this);
   
}

void cfdGAserver::getInfoFromChangeManager()
{
   this->cfdpackage = (this->changemanager).getCFDpackageName();
   this->newvtkfilename = (this->changemanager).getNewVtkFileName();

   this->checkedgene = (this->changemanager).getCheckedGene();
   this->directory = (this->changemanager).getDirectoryName();
  
   this->popsize = (this->changemanager).getPopSize();
   this->totalruns = (this->changemanager).getTotalRuns();
   this->initialtype = (this->changemanager).getInitialType();
   this->totalmatingevents = (this->changemanager).getTotalMatingEvents();
   this->initialpopfilename = (this->changemanager).getInitialPopFileName();
   this->boundaryfilename = (this->changemanager).getBoundaryFileName();

}

void cfdGAserver::setCFDpackage(char* name)
{

}

void cfdGAserver::setNewVTKFileName(char* name)
{

}

void cfdGAserver::setCheckedGene(int value)
{
   this->checkedgene = value;
}

void cfdGAserver::sendVtkFileToVRserver(char* vrservername, char* vtkfilename)
{  
  vtkUnstructuredGridReader  *ugrid = vtkUnstructuredGridReader::New();
   vtkSocketCommunicator *sock = vtkSocketCommunicator::New();
   (this->ugrid)->SetFileName(vtkfilename);
   (this->ugrid)->Update();
   
   if((this->sock)->ConnectTo(this->VeApp_VRserver_name,33000))
   {
      if(!(this->sock)->Send((this->ugrid)->GetOutput(),1,9))
      {
         std::cerr<<"Server error::Error sending data."<<std::endl;
      
         if(this->sock!=0)
         {
            this->sock->CloseConnection();
         }
         
      }
     
   else
   {
      std::cerr<<"[ERR] Unable to connect to the server"<<std::endl;
   }

}


void cfdGAserver::initialpops()
{
   this->genechain = new cfdGAManager(this->paramfilename);
   std::cout<<"The initialzation of pops is finished"<<std::endl;

}
void cfdGAserver::evolve()
{
   if(this->popsize>3)
   {
      for(int run=0;run<this->totalruns;run++)
      {
         this->initialpops();
         this->currentrun = run;
      
         for(int i=0;i<popsize;i++)
         {
            this->genechain->ga[i]->evolve();
         }

         for (int mev=0; mev<=this->totalmatingevents; mev++)
         {
            this->currentmatingevent = mev;
            this->genechain->full_sort();
            /*
            *  need to block this function
            */
            if(this->currentfitness.size()>0)
            {
               for(int i=0; i<popsize;i++)
               {
                  this->currentfitness.pop_back();
               }
            }
            for(int i=0; i<popsize;i++)
            {
               this->currentfitness.push_back(this->genechain->ga[i]->fitness);
            }
            

            int p1,p2;//two chosen parents

            p1= this->genechain->rouletteWheelSelector();

            do
            {
               p2 = this->genechain->rouletteWheelSelector();
            } while(p1==p2);

            int c1,c2;//two children
         
            c1 = this->genechain->randomReplaceSelector();
         
            do
            {
               c2 = this->genechain->randomReplaceSelector();
            } while(c1==c2);

            int mut1, mut2;

            mut1 =this->genechain->randomReplaceSelector();
            do
            {
               mut2 = this->genechain->randomReplaceSelector();

            } while(mut1 == mut2);

            this->genechain->randomReplace(p1, p2, c1,c2,mut1,mut2);

      
            for(int i=0; i<popsize;i++)
            {
               this->genechain->ga[i]->evolve();
            }

         }
      
      //this->genechain->full_sort();
      //this->genechain->save_final(run);
      //this->genechain->report(stats, run);
      
         delete this->genechain;
     
      }
   }
   
}

void cfdGAserver::singleCase(float* value, bool displayactived)
{
   cfdCalculator* singlecasecalculator;
   
   singlecasecalculator = new cfdCalculator(value, displayactived);
}

float cfdGAserver::getMutationRate()
{
   return this->mutationrate;
}

float cfdGAserver::getCrossoverRate()
{
   return this->crossoverrate;
}

float cfdGAserver::getMaxFitness()
{
   return this->currentfitness[this->popsize-1];
}

float cfdGAserver::getMinFitness()
{
   return this->currentfitness[0];
}

int cfdGAserver::getCurrentRun()
{
   return this->currentrun;
}

int cfdGAserver::getCurrentMatingEvent()
{
   return this->currentmatingevent;
}
