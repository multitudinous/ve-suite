#include "cfdVeModel.h"

cfdVeModel::cfdVeModel(cfdVeController* controller, char* filename)
{
   this->changemanager = cfdChangeManager::getInstance();
   this->chanegmanager->setInfoFromParamFile(filename);
    
   this->regularOPFunc = new vpr::ThreadMemberFunctor<cfdVeModel>(this, &cfdVeModel::runRegularOPThread);
   this->OPTh[0] = new vpr::Thread(this->regularOPFunc);
     
   this->_modelsubject =controller;
   this->_modelsubject->addObserver(this);
    
}

cfdVeModel::~cfdVeModel()
{
   delete this->_modelsubject;
   
}


void cfdVeModel::changeChecking()
{
   if(this->cfdpackage!=(this->changemanager).getCFDpackageName())
   {
      
   }

}

/*
 * The reason to create a sperated thread for regular OP, is because this operation takes time
 */
void cfdVeModel::runRegularOPThread(void* unused)
{
   this->GAserver = new cfdGAserver();
   this->GAserver->getInfoFromChangeManager();
   this->GAserver->evolve();

   delete this->GAserver;
}

void cfdVeModel::runInteraciveDesignSaveOP(void* unused)
{
   /*
    *    when the "save" button in the interactivedesign section is actived
    *    the new design candidates are saved in the DATA/design.dat
    */

   std::ofstream designcandidatesfile;
   cfdDirectoryHandle* directoryhandler;
   char* directory;
   
   directoryhandler = new cfdDirectoryHandler();
   directory = directoryhandler->getDirectory();
   directoryhandler->changeToNewWorkingDirectory("DATA");
   
   designcandidatesfile.open("design.dat",ios::app);
   
   float* localdesignparams;

   localdesignparams = new float[(this->changemanager).getNumDesignParams()];

   for(int i=0; i<(this->changemanager).getNumDesignParams();i++)
   {
       designcandidatesfile<<((this->changemanager).getCurrentDesignParams())[i];
       designcandidatesfile<<" ";
      
   }
   designcandidatesfile<<std::endl;

   directoryhandler->changeToOldWorkingDirectory(directory);
   
   delete directoryhandler;
   
}

void cfdVeModel::runInteractiveDesignDisplayOP(void* unused)
{
   float* localdesignparams;
   localdesignparams = new float[(this->changemanager).getNumDesignParams()];
   for(int i=0; i<(this->changemanager).getNumDesignParams();i++)
   {  
      localdesignparams[i]= ((this->changemanager).getDesignParams())[i];
   }
   
   this->interactiveGAserver = new cfdGAserver();
   
   this->interactiveGAserver->singlercase(localdesignparams, interactivedisplayactived);
   notifyObserver();
   delete localdesignparams;
   delete this->interactiveGAserver;
   
}

void cfdVeModel::runInteractiveGAUpdateOP(void* unused)
{
   /*
    * send the current GA information back to GUI
    */
   float mutationrate;
   float crossoverrate;
   float maxfitness;
   float minfitness;
   int   currentrun;
   int   currentmatingevent;
   
   mutationrate = this->GAserver->getMutationRate();
   crossoverrate = this->GAserver->getCrossoverRate();
   maxfitness = this->GAserver->getMaxFitness();
   minfitness = this->GAserver->getMinFitness();
   currentrun = this->GAserver->getCurrentRun();
   currentmatingevent = this->GAserver->getCurrentMatingEvent();

   /*
    * need Dr.K 's help, so that VE_Conductor can get these data
    */

   
}

void cfdVeModel::runInteractiveGAGeneCheckOP(void* unused)
{
   this->interactiveGAserver = new cfdGAserver();
   this->checkedgene = (this->changemanager).getCheckedGene();
   float* checkeddesignparams;
   int designparanum;

   designparamnum = (this->changemanager).getNumDesignParams();
   
   checkeddesignparams = new float[designparamnum];
   
   mValueLock.acquire();
   {
      for(int i=0; i<designparamnum;i++)
      {
         checkeddesignparams[i] = this->GAserver->genechain->ga[checkedgene-1]->gaParams[i]; 
      }
   }
   mValueLock.release();
   
   this->interactiveGAserver->singlercase(checkeddesignparams, interactivedisplayactived);
   notifyObserver();
   delete checkeddesignparams;
   delete this->interactiveGAserver;


}

void cfdVeModel::update(cfdSubjectBase* theChangedSubject)
{
   if(theChangeSubject == (this->_modelsubject))
   {
      this->opID = (this->changemanager).getOpID();

      switch (this->opID)
      {
         case INTERACTIVE_DESIGN_SAVE:
            {
               this->interactiveOPFunc = new vpr::ThreadMemberFunctor<cfdVeModel>(this, &cfdVeModel::runInteractiveDesignSaveOP);
               this->OPTh[1] = new vpr::Thread(this->interactiveOPFunc);
               
               break;
            }
         case INTERACTIVE_DESIGN_DISPLAY:
            {
               this->interactiveOPFunc = new vpr::ThreadMemberFunctor<cfdVeModel>(this, &cfdVeModel::runInteractiveDesignDisplayOP);
               this->OPTh[1] = new vpr::Thread(this->interactiveOPFunc);

               break;
            }
         case INTERACTIVE_GA_INFO_UPDATE:
            {
               
               this->interactiveOPFunc = new vpr::ThreadMemberFunctor<cfdVeModel>(this, &cfdVeModel::runInteractiveGAUpdateOP);
               this->OPTh[1] = new vpr::Thread(this->interactiveOPFunc);
break;
            }
         case INTERACTIVE_GA_GENE_CHECK:
            {
               
              this->interactiveOPFunc = new vpr::ThreadMemberFunctor<cfdVeModel>(this, &cfdVeModel::runInteractiveGAGeneCheckOP);
              this->OPTh[1] = new vpr::Thread(this->interactiveOPFunc);
 
               break;
            }
         default:
            {
               break;
            }
      }
      
   }

}


