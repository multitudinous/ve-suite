#include "cfdGAFactory.h"

cfdGAFactory::cfdGAFactory(char* filename)
{
   this->param_file =cfdVeReadParamFactory::getInstance().getParamFile(filename);
   
   this->initialtype = this->param_file->getInitialType();   
   this->numparamspergene = this->param_file->getNumDesignParams();
   this->popsize = this->param_file->getPopSize();
   this->gaboundaryfilename = this->param_file->getBoundaryFileName();
   this->gaparamsfilename = this->param_file->getInitialPopFileName();
    
   std::ifstream gaBoundaryFile;
   
   gaBoundaryFile.open(this->gaboundaryfilename);

   this->galowerlimit =new double[this->numparamspergene];
   this->gaupperlimit =new double[this->numparamspergene];

   for (int i=0;i<this->numparamspergene; i++)
   {
      gaBoundaryFile>>this->galowerlimit[i];
      gaBoundaryFile>>this->gaupperlimit[i];
   }

   gaBoundaryFile.close();
   
   switch(initialtype)
   {
      case 0://read initial population from from the existed file
         this->initializeFromFile( this->popsize );
         break;
      case 1://computer randomly generates the initial population 
         this->initializeFromComputer( this->popsize );
         break;
      case 2://combine the above two methods
         this->initializeFromMixed( this->popsize );
         break;
      default:
         this->initializeFromComputer( this->popsize );
         break;
   }

}

cfdGAFactory::~cfdGAFactory()
{
   int temp = this->ga.size();
   for(int i=0; i<temp;i++)
   {
      delete this->ga[i];
   }
}

void cfdGAFactory::initializeFromFile(int popsize)
{

   double temp;
   std::ifstream gaparamsfile;
   gaparamsfile.open(this->gaparamsfilename);

   int genecount = this->ga.size();
   std::cout<<"before read file, geneCount= "<<genecount<<std::endl;
   for(int i=genecount; i<(popsize+genecount); i++)
   {
      if(!gaparamsfile.eof())
      {
         this->ga.push_back(new cfdGABase(this->numparamspergene));
         for (int j=0; j<this->numparamspergene; j++)
         {
            gaparamsfile>>temp;
            
            if(temp<this->galowerlimit[j])
               temp = this->galowerlimit[j];
            else if (temp>this->gaupperlimit[j])
               temp = this->gaupperlimit[j];
            
            (this->ga[i])->gaParams[j] = temp;
         }


      }
      else
      {
         this->ga.pop_back();
         break;
      }
          
   }

   for(int i=genecount; i<(popsize+genecount);i++)
   {
      this->ga[i] = false;
   }

   gaparamsfile.close();

   std::cout<<"after read file, geneCount ="<<this->ga.size()<<std::endl;

   int randgenerated = popsize-this->ga.size();
   
   if(randgenerated>0)
      initializeFromComputer(randgenerated);   
}

void cfdGAFactory::initializeFromComputer(int popsize)
{
   //std::cout<<"the number of randomly generated genes is "<< popsize <<std::endl;
   int genecount = this->ga.size();
   for(int i=genecount; i<(popsize+genecount);i++)
   {
      this->ga.push_back(new cfdGABase(this->numparamspergene));
      for(int j=0; j<this->numparamspergene;j++)
      {
         (this->ga[i])->gaParams[j]=(this->gaupperlimit[j]-this->galowerlimit[j])*drand48()+this->galowerlimit[j];
      }

   }

   /*for(int i=geneCount;i<(popsize+geneCount);i++)
   {
      for(int j=0; j<this->nParamsPerGene;j++)
      {
         std::cout<<"temp = "<<(this->ga[i])->gaParams[j]<<"  ";
      }

      std::cout<<std::endl;
   }*/

   for(int i=genecount; i<(popsize+genecount);i++)
   {
      this->ga[i] = false;
   }

}

void cfdGAFactory::initializeFromMixed(int popsize)
{

}
