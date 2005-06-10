#ifdef VE_PATENTED
#ifdef _OSG
#include <osg/StateSet>
#include <osg/Shader>

#ifdef CFD_USE_SHADERS
#include <osgNVCg/Context>
#include <osgNVCg/Program>
#include <osgNVCg/CgGeometry>
#endif

#include "cfdOSGShaderManager.h"
//////////////////////////////////////////
//Constructors                          //
//////////////////////////////////////////
cfdOSGShaderManager::cfdOSGShaderManager()
{
   _shaderDirectory = 0;
   _bounds = 0;
   _tUnit =0;
   _useGLSL = true;
}
///////////////////////////////////////////////////////////////////////
cfdOSGShaderManager::cfdOSGShaderManager(const cfdOSGShaderManager& sm)
{
   _useGLSL = sm._useGLSL;
   _ss = sm._ss;
   _fshader = new osg::Shader(*sm._fshader);
   _vshader = new osg::Shader(*sm._vshader);
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
///////////////////////////////////////////
void cfdOSGShaderManager::UseCG(bool useCG)
{
   _useGLSL = (useCG)?false:true;
}
#ifdef CFD_USE_SHADERS
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
      _ss->setAttributeAndModes(fprog,osg::StateAttribute::ON/*|osg::StateAttribute::OVERRIDE*/);
   }
}
#endif
//////////////////////////////////////////////////////////////////////////////////////
osg::Shader* cfdOSGShaderManager::_createGLSLShaderFromFile(const std::string filename,
                                                           bool isFrag)
{
   if(isFrag){
      _fshader = new osg::Shader(osg::Shader::FRAGMENT);
      _fshader->loadShaderSourceFromFile(filename.c_str());
      return _fshader.get();
   }else{
      _vshader = new osg::Shader(osg::Shader::VERTEX);
      _vshader->loadShaderSourceFromFile(filename.c_str());
      return _vshader.get();
   }
   return 0;
}
/////////////////////////////////////////////////////////////////////////
void cfdOSGShaderManager::_setupGLSLShaderProgram(osg::StateSet* ss,
                                             osg::Program* glslProgram,
                                             const std::string pgName)
{
   if(ss){
      if(glslProgram){
         glslProgram->setName(pgName.c_str());
         ss->setAttributeAndModes(glslProgram,osg::StateAttribute::ON);
      }
   }
}
//////////////////////////////////////////////////
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
//not fully implemented!!!!!!!!!!!!!!!!!!!!!!!!!
cfdOSGShaderManager& cfdOSGShaderManager::operator=(const
	                                            cfdOSGShaderManager& sm)
{
   if(this != &sm){
      _ss = sm._ss;
      SetShaderDirectory(sm._shaderDirectory);
   }
   return *this;
}
#endif //_OSG
#endif
