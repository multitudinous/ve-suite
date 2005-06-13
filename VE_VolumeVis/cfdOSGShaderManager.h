#ifndef CFD_OSG_SHADER_MANAGER_H
#define CFD_OSG_SHADER_MANAGER_H
#ifdef VE_PATENTED
#ifdef _OSG
#include <osg/StateSet>
namespace osg{
   class Shader;
}
class cfdOSGShaderManager{
public:
   cfdOSGShaderManager();
   cfdOSGShaderManager(const cfdOSGShaderManager& sm);
   virtual ~cfdOSGShaderManager();

   virtual void Init() = 0;

   void SetShaderDirectory(char* dir);
   void SetBounds(float* bounds);
   void UseCG(bool useCG = false);

   osg::StateSet* GetShaderStateSet();
   unsigned int GetAutoGenTextureUnit(){return _tUnit;}

protected:
   virtual cfdOSGShaderManager& operator=(const cfdOSGShaderManager& sm);

   //////////////////
   //GLSL interface//
   //////////////////
   virtual void _setupGLSLShaderProgram(osg::StateSet* ss,
                                        osg::Program* glslProgram,
                                        const std::string pgName,bool override = false);
   virtual osg::Shader* _createGLSLShaderFromFile(const std::string filename,
                                                           bool isFrag);
   virtual void _setupStateSetForGLSL()=0;

   osg::ref_ptr<osg::Shader> _vshader;
   osg::ref_ptr<osg::Shader> _fshader;

   osg::ref_ptr<osg::StateSet> _ss;
   char* _shaderDirectory;
   unsigned int _tUnit;
   float* _bounds;
   bool _useGLSL;
};
#endif //_OSG
#endif
#endif// CFD_OSG_SHADER_MANAGER_H
