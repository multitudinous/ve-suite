#include "cfdGABase.h"
cfdGABase::cfdGABase(int n)
{
   this->score =0.0;
   this->fitness =0.0;
   this->evaluated =false;
   this->neval =0;
   this->eval =0; 
   this->evd =0;
   this->nParams =n;
   this->gaParams =new float[nParams];
   
}


cfdGABase::~cfdGABase()
{

}

// The Gaussian mutator picks a new value based on a Gaussian distribution
// around the current value.  We respect the bounds (if any).
//*** need to figure out a way to make the stdev other than 1.0 

int* cfdGABase::mutator()
{
   int ran1; //the number of parameter is going to be mutated
   int *ran2; //the nth parameter is going to be mutated  

   //std::cout<< " the number of parameter would be mutated "<< ran1 << std::endl;
   int temp =this->nParams;
   
   ran1 = lrand48()%temp;
      
   ran2 = new int[ran1];

   for (int i=0; i<ran1; i++)
   {
     ran2[i] = lrand48()%(this->nParams); 
   }
  
   //std::cout<<" the parameter that should be mutated are the following: ";
   /*for(int i=0; i<ran1; i++)
   {
      std::cout << ran2[i] << "  ";
   }*/
  
   //std::cout << std::endl;
   
   //this->mutr = drand48()-0.05;//could be changed to Gaussian mutator
   this->mutr = GaussianMutator(); 
   this->numMutr = ran1;
   
   return ran2;
}

double cfdGABase::GaussianMutator()
{
   double rsquare, factor, var1, var2;
   do
   {
      var1 = 2.0 * drand48() - 1.0;
      var2 = 2.0 * drand48() - 1.0;
      rsquare = var1*var1 + var2*var2;
   } while(rsquare >= 1.0 || rsquare == 0.0);

   double val = -2.0 * log(rsquare) / rsquare;
   if(val > 0.0) 
      factor = sqrt(val);
   else
     factor = 0.0;	

   //cachevalue = var1 * factor;
   //cached = gaTrue;

   return (var2 * factor);
}

void cfdGABase :: crossover()
{

}


void cfdGABase::evolve()
{
  if(this->evaluated == false) 
  {
     this->neval++;
     this->findfit();
     this->evaluated = true;
  }   
}


void cfdGABase :: comparator()
{


}

void cfdGABase :: findfit()
{
   if(this->evaluated == false)
   {
      this->designEvaluator(this->gaParams);

   }
   

}

void cfdGABase :: designEvaluator(float *gaParams)
{
      //test case f(xi) = 1/(x1*x1+x2*x2+x3*x3+x4*x4+1);
   /*this->fitness = 1/(this->gaParams[0]*this->gaParams[0]+this->gaParams[1]*this->gaParams[1]+this->gaParams[2]*this->gaParams[2]+this->gaParams[3]*this->gaParams[3]+1);*/
     
      this->mCalculator = new cfdCalculator(gaParams);
      std::ifstream fitnessfile;
      fitnessfile.open("finalfitness.dat");
      float temp;
      fitnessfile>>temp;
      this->fitness = temp;
      fitnessfile.close();


}


