#ifndef CFD_VEREADPARAM_FACTOR_H
#define CFD_VEREADPARAM_FACTOR_H

#include <map>
#include <string>

class cfdVeReadParam;

class cfdVeReadParamFactory
{
   public:

      /**
       * Returns the sole instance of this singleton
       */
      static cfdVeReadParamFactory& getInstance();

      /**
       * Returns the requested ParamReader; will load the ParamReader if it
       * does not already exist.
       *
       * @param   filename    the name of the file for this ParamReader to parse.
       *
       * @return     a cfdVEReadParam object associated with filename.
       */
      cfdVeReadParam* getParamFile(char *);

   private:
      
      ///Copyright
      cfdVeReadParamFactory()
      {}
      ~cfdVeReadParamFactory();
      /*cfdVeReadParamFactory(const cfdVeReadParamFactory& src);
      cfdVeReadParamFactory& operator=(const cfdReadParamFactory& rhs);*/

      ///the instance of this singleton
      static cfdVeReadParamFactory* myinstance;
      
      ///all of the parameter files that have been opened
      std::map<std::string, cfdVeReadParam*> mFileMap;

      void otherOperations();

      
};

#endif
