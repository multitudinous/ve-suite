#include "cfdGAManager.h"


//just use this to calculate the cfd results and show it in real time.
cfdGAManager :: cfdGAManager(char* filename) : cfdGAFactory(filename)
{
}
   
cfdGAManager::~cfdGAManager()
{

}

void cfdGAManager :: selector( int type)
{
 /*DESCRIPTION:
  The selectors are functions that use information in fitness objects to pick
genomes from a population.  There are a number of selection criteria, and
not every selection method will work with every fitness object.*/
   
/* Brief introduction to the different selection method
   Rank - pick the genome with the best fitness (not objective score)
   RouletteWheel - weighted selection where individuals with better fitness have
                a greater chance of being selected than those with lower 
                scores.
   Tournament - similar to roulette, but instead of choosing one, choose two
                then pick the better of the two as the selected individuals
   Uniform - stochastic uniform selection picks randomly from the 
                population.  Each individual has as much chance as any other.
   SRS - stochastic remainder selection does a preselection based on 
                the expected number of each genome, then a random sampling on 
                the preselected list.
   DS - deterministic sampling is implemented as described in 
                Goldberg's book (as much as I could understand it, anyway).*/

   switch (type)
   {
      case 1:
         this->rankSelector();// pick the genome with the best fitness (not objective score)
         break;
         
      case 2:
         this->rouletteWheelSelector();//weighted selection where individuals with better fitness have
                                 //a greater chance of being selected than those with lower scores.
         break;
         
      case 3:
         this->tournamentSelector();// similar to roulette, but instead of choosing one, choose two
                              // then pick the better of the two as the selected individuals
         break;
         
      case 4:
         this->uniformSelector();//stochastic uniform selection picks randomly from the 
                           //population.  Each individual has as much chance as any other.
         break;
         
      default:
         this->rouletteWheelSelector();
         break;
   }
   

}

void cfdGAManager :: findTotalFitness()
{
   this->total_fitness = 0.0;
   for(int i=0; i<(this->ga.size());i++)
   {
      this->total_fitness += this->ga[i]->fitness;

   }
         

}

double cfdGAManager :: getTotalFitness()
{
   this->findTotalFitness();
   return this->total_fitness;
}

void cfdGAManager :: rankSelector()
{
   
}

int cfdGAManager :: rouletteWheelSelector()
{
  
   double ttl,dart;  //total fitness and dart for selection
   int i;            //loop index
   ttl = this->getTotalFitness();
   
   //Select a fitness in the range 0..ttl
   i=0;  //point to the first number
   dart=ttl*drand48()-this->ga[0]->fitness;  //throw the dart, initializing 
                              //for the first number
   while(dart>0)dart-=this->ga[++i]->fitness;//figure out where the dar hit
   return(i);

}

/*void cfdGAManager :: max_min()
{
   double min, max;
   int min_index,max_index;
   min =this->ga[0]->fitness;
   max =this->ga[0]->fitness;

   min_index =0;
   max_index =0;
   for (int i=1; i<this->ga.size();i++)
   {
      if(min>this->ga[i]->fitness)
      {
         min = this->ga[i]->fitness;
         min_index = i;
      }
      if(max<this->ga[i]->fitness)
      {
         max = this->ga[i]->fitness;
         max_index = i;
      }

   }
   

}*/

void cfdGAManager :: full_sort()
{
   
   int flag;
   
   do
   {
      flag=0;
      for(int i=0;i<this->ga.size()-1;i++)
      {
         if(this->ga[i]->fitness>this->ga[i+1]->fitness)
         {
            std::swap (this->ga[i], this->ga[i+1]);
            flag =1;
         }
    }
  } while(flag);

}

   


void cfdGAManager :: tournamentSelector()
{
   
}

void cfdGAManager :: uniformSelector()
{
   
}

void cfdGAManager :: crossover(int p1, int p2, int c1, int c2)
{
//randomly generate the crossover point in the array

   int ran;//one point crossover
   int paramsCounter;
   paramsCounter = this->numParams;
   ran=lrand48()%(paramsCounter);

   //std::cout<<"Crossover point is "<<ran<<std::endl;

   for(int i=0; i<ran;i++)
   {
     this->ga[c1]->gaParams[i]= this->ga[p1]->gaParams[i];
     this->ga[c2]->gaParams[i]= this->ga[p2]->gaParams[i];
      
   }

   for(int i=ran;i<numParams;i++)
   {
      this->ga[c1]->gaParams[i]= this->ga[p2]->gaParams[i];
      this->ga[c2]->gaParams[i]= this->ga[p1]->gaParams[i];
   }

   this->ga[c1]->evaluated = false;
   this->ga[c2]->evaluated = false;
      

}

void cfdGAManager :: mutator(int c1)
{
   int *ran;
   this->ga[c1]->evaluated=false;
   ran = this->ga[c1]->mutator();
   for(int i=0; i<this->ga[c1]->numMutr;i++)
   {
      
      for(int j=0;j<this->numParams;j++)
      {
         if(j==ran[i])
         {
            this->ga[c1]->gaParams[j]+=this->ga[c1]->mutr;
            if(this->ga[c1]->gaParams[j]<this->galowerlimit[j])
               this->ga[c1]->gaParams[j] = this->galowerlimit[j];
            else if(this->ga[c1]->gaParams[j]>this->gaupperlimit[j])
               this->ga[c1]->gaParams[j] = this->gaupperlimit[j];
         }

      }      
         
   }
   
   this->ga[c1]->evaluated = false;
   
}
   
void cfdGAManager :: breed()
{

}

int cfdGAManager :: randomReplaceSelector()//randomly choose two genes but the two most fit one
{
   int ran;
   do
   {
      ran = lrand48()%(this->ga.size());
   }while(ran>=(this->ga.size()-2));

   return ran;
}

void cfdGAManager :: randomReplace(int p1, int p2,int c1, int c2, int mut1, int mut2)
{
   
   this->ga.push_back(new cfdGABase(this->numParams));//for children
   this->ga.push_back(new cfdGABase(this->numParams));

   this->crossover(p1, p2, this->ga.size()-2, this->ga.size()-1);
   
   std::swap(this->ga[c1], this->ga[this->ga.size()-2]);
   std::swap(this->ga[c2], this->ga[this->ga.size()-1]);
   //std::cout<<"p1 ="<<p1<<" p2 ="<<p2<<" c1="<<c1<<" c2="<<c2<<std::endl;

   this->mutator(mut1);
   this->mutator(mut2);

   this->ga.pop_back();
   this->ga.pop_back();
}


void cfdGAManager :: rouletteWheelReplace()
{
   
}

void cfdGAManager :: randomEliteReplace()
{

}

/*void cfdGAManager :: report(ostream &aus,int g)//report fitness tracks
{
   int popsize;
   double sg,mu;
   sg=mu=0.0;
   popsize = this->ga.size();
   for(int i=0;i<popsize;i++)
   {
      mu+=this->ga[i]->fitness;
      sg+=(this->ga[i]->fitness*this->ga[i]->fitness);
   }
   mu/=popsize;
   sg=sg/popsize-mu*mu;
   if(sg<0)
      sg=0; 
   else 
      sg=sqrt(sg);
   
   std::cout << g << " " << mu << " " << sg << '\n';
   aus << mu << " " << sg << '\n';
   //avg[g]+=mu;
   //dev[g]+=mu*mu;

}*/

/*void cfdGAManager :: save_final(int r) //write out the population
{
   fstream aus;
   char fn[60];
   int popsize;
   
   popsize = this->ga.size();
   
   sprintf(fn,"pop%d.aut",r);
   aus.open(fn,ios::out);
   
   for(int i=0; i<popsize; i++)
   {
      aus<<i<<"  ";
      for (int j=0; j<this->nParamsPerGene;j++)
      {
         aus<<this->ga[i]->gaParams[j]<<"  ";
      }

      aus<<this->ga[i]->fitness<<'\n';
   }
   aus.close();

}*/


