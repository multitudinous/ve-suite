#ifndef CFD_OSG_SHADER_MANAGER_H
#define CFD_OSG_SHADER_MANAGER_H

#ifdef _OSG
#include <osg/StateSet>
#ifdef CFD_USE_SHADERS

class cfdOSGShaderManager{
public:
   cfdOSGShaderManager();
   cfdOSGShaderManager(const cfdOSGShaderManager& sm);
   virtual ~cfdOSGShaderManager();

   virtual void Init() = 0;

   void SetShaderDirectory(char* dir);
   osg::StateSet* GetShaderStateSet();
   virtual cfdOSGShaderManager& operator=(const cfdOSGShaderManager& sm);
protected:
   void _setupCGShaderProgram(osg::StateSet* ss,
		                      char* progName,
			                     char* funcName);
   osg::ref_ptr<osg::StateSet> _ss;
   char* _shaderDirectory;
};

#endif //CFD_USE_SHADERS
#endif //_OSG
#endif// CFD_OSG_SHADER_MANAGER_H
