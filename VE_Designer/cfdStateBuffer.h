#ifndef CFD_STATEBUFFER_H
#define CFD_STATEBUFFER_H


class cfdStateBuffer
{
   public:
      cfdStateBuffer() {}
      ~cfdStateBuffer(){}

   private:

      bool interactiveGA;
      double* designparams;
      int numdesignparams;
      int activedesingparams;

      int totalruns;
      int totalmatingeven;
      int popsize;
      int currun;
      int curmev;
      int curgene;

      
      double averagefitness;
      double maxfitness;
      double minfitness;

      vpr::Mutex     mValueLock;

};

#endif
