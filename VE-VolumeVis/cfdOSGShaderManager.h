#ifndef CFD_OSG_SHADER_MANAGER_H
#define CFD_OSG_SHADER_MANAGER_H

#ifdef _OSG
#include <osg/StateSet>
#ifdef CFD_USE_SHADERS
#include <osgNVCg/Program>

class cfdOSGShaderManager{
public:
   cfdOSGShaderManager();
   cfdOSGShaderManager(const cfdOSGShaderManager& sm);
   virtual ~cfdOSGShaderManager();

   virtual void Init() = 0;

   void SetShaderDirectory(char* dir);
   osg::StateSet* GetShaderStateSet();
   void SetBounds(float* bounds);
   unsigned int GetAutoGenTextureUnit(){return _tUnit;}
   osgNVCg::Program* GetVertexProgram();
   osgNVCg::Program* GetFragmentProgram();

   virtual cfdOSGShaderManager& operator=(const cfdOSGShaderManager& sm);
protected:
   virtual void _setupCGShaderProgram(osg::StateSet* ss,
		                             char* progName,
			                          char* funcName);
   osg::ref_ptr<osgNVCg::Program> _vert;
   osg::ref_ptr<osgNVCg::Program> _frag;
   osg::ref_ptr<osg::StateSet> _ss;
   char* _shaderDirectory;
   unsigned int _tUnit;
   float* _bounds;
};

#endif //CFD_USE_SHADERS
#endif //_OSG
#endif// CFD_OSG_SHADER_MANAGER_H
