#ifdef VE_PATENTED
#ifdef _OSG 
#include <osg/StateSet>
#ifdef CFD_USE_SHADERS
#include <osgNVCg/Context>
#include <osgNVCg/Program>
#include <osgNVCg/CgGeometry>
#include "cfdOSGShaderManager.h"
//////////////////////////////////////////
//Constructors                          //
//////////////////////////////////////////
cfdOSGShaderManager::cfdOSGShaderManager()
{
   _shaderDirectory = 0;
   _bounds = 0;
   _tUnit =0;
}
///////////////////////////////////////////////////////////////////////
cfdOSGShaderManager::cfdOSGShaderManager(const cfdOSGShaderManager& sm)
{
   _ss = sm._ss;
   SetShaderDirectory(sm._shaderDirectory);
}
///////////////////////////////////////////
//Destructor                             //
///////////////////////////////////////////
cfdOSGShaderManager::~cfdOSGShaderManager()
{
   if(_bounds){
      delete [] _bounds;
      _bounds = 0;
   }
}
/////////////////////////////////////////////////////////
osgNVCg::Program* cfdOSGShaderManager::GetVertexProgram()
{
   if(_vert.get()){
      return _vert.get();
   }
   return 0;
}
///////////////////////////////////////////////////////////
osgNVCg::Program* cfdOSGShaderManager::GetFragmentProgram()
{
   if(_frag.valid()){
      return _frag.get();
   }
   return 0;
}
////////////////////////////////////////////////////////////////////////
void cfdOSGShaderManager::_setupCGShaderProgram(osg::StateSet *ss, 
                                              char* fragProgramFileName,
                                              char* fragFunctionName)
{
   if(_ss.valid()){
      // create fragment program
      osgNVCg::Program *fprog = new osgNVCg::Program(osgNVCg::Program::FP30);
      fprog->setFileName(fragProgramFileName);
      fprog->setEntryPoint(fragFunctionName);
      fprog->setUseOptimalOptions(true);

      //apply the shaders to state set
      _ss->setAttributeAndModes(fprog,osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);
   }
}
void cfdOSGShaderManager::SetBounds(float* bounds)
{
   if(!_bounds){
      _bounds = new float[6];
   }
   _bounds[0] = bounds[0];
   _bounds[1] = bounds[1];
   _bounds[2] = bounds[2];
   _bounds[3] = bounds[3];
   _bounds[4] = bounds[4];
   _bounds[5] = bounds[5];

}
///////////////////////////////////////////////////////
osg::StateSet* cfdOSGShaderManager::GetShaderStateSet()
{
   if(_ss.valid()){
      return _ss.get();
   }
   return 0;
}
///////////////////////////////////////////////////////////
void cfdOSGShaderManager::SetShaderDirectory(char* shadDir)
{
   if(_shaderDirectory){
      delete [] _shaderDirectory;
      _shaderDirectory = 0;
   }
   _shaderDirectory = new char[strlen(shadDir)+1];
   strcpy(_shaderDirectory,shadDir);
}

//////////////////////////////////////////////////////////
//equal operator                                        //
//////////////////////////////////////////////////////////
cfdOSGShaderManager& cfdOSGShaderManager::operator=(const
	                                            cfdOSGShaderManager& sm)
{
   if(this != &sm){
      _ss = sm._ss;
      SetShaderDirectory(sm._shaderDirectory);
   }
   return *this;
}
#endif //CFD_USE_SHADERS
#endif //_OSG
#endif 
