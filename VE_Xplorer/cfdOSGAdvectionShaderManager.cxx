#ifdef _OSG
#include <osg/Texture3D>
#include <osg/Texture1D>
#include <osg/TexEnv>
#include <vector>
#include <iostream>
#ifdef CFD_USE_SHADERS
#include <osgNVCg/Context>
#include <osgNVCg/Program>
#include <osgNVCg/CgGeometry>
#include "cfdOSGAdvectionShaderManager.h"
#include "cfdUpdateableOSGTexture1d.h"
#define PI  3.1416
////////////////////////////////////////////////////////////
//Constructors                                            //
////////////////////////////////////////////////////////////
cfdOSGAdvectionShaderManager::cfdOSGAdvectionShaderManager()
:cfdOSGShaderManager()
{
   //_deltaT = 1;
   _reinit = true;
   //_period = 1;
  // _time = 0;

   _noiseScaleCallback =0 ;
   _deltaCallback = 0 ;
   _timeCallback =0 ;
   _periodCallback = 0;
   _dyeScaleCallback =0 ;
   _dyeTransCallback = 0;
   _noiseCbk = 0;
   _fieldSize[0] = 0;
   _fieldSize[1] = 0;
   _fieldSize[2] = 0;

   /*_dyeScale[0] = 1;
   _dyeScale[1] = 1;
   _dyeScale[2] = 1;
   _noiseScale[0] = 1;
   _noiseScale[1] = 1;
   _noiseScale[2] = 1;

   _dyeTranslation[0] = 0;
   _dyeTranslation[1] = 0;
   _dyeTranslation[2] = 0;*/
}
////////////////////////////////////////////////////////////////
cfdOSGAdvectionShaderManager::cfdOSGAdvectionShaderManager(const
     	                cfdOSGAdvectionShaderManager& sm)
:cfdOSGShaderManager(sm)
{
   //_period = sm._period;
   //_time = sm._time;
   //only want a pointer here
   _velocity = sm._velocity;
   //new these
   _propertyToAdvect = new osg::Texture3D(*(sm._propertyToAdvect.get()));
   _weightW = new osg::Texture3D(*(sm._weightW.get()));
   _weightV = new osg::Texture3D(*(sm._weightV.get()));
   _lookUpFunction = new osg::Texture1D(*(sm._lookUpFunction.get()));
   _noiseCbk = new cfdUpdateableOSGNoiseTexture3d(*sm._noiseCbk);
   _dye = new osg::Texture3D(*(sm._dye.get()));
   _reinit = sm._reinit;
   //_deltaT= sm._deltaT;
   _fieldSize[0] = sm._fieldSize[0];
   _fieldSize[1] = sm._fieldSize[1];
   _fieldSize[2] = sm._fieldSize[2];

   _noiseScaleCallback = new cfdUpdateParameterCallback(*sm._noiseScaleCallback);
   _deltaCallback = new cfdUpdateParameterCallback(*sm._deltaCallback);
   _timeCallback = new cfdUpdateParameterCallback(*sm._timeCallback);
   _periodCallback = new cfdUpdateParameterCallback(*sm._periodCallback);
   _dyeScaleCallback = new cfdUpdateParameterCallback(*sm._dyeScaleCallback);
   _dyeTransCallback = new cfdUpdateParameterCallback(*sm._dyeTransCallback);

   /*_dyeScale[0] = sm._dyeScale[0];
   _dyeScale[1] = sm._dyeScale[1];
   _dyeScale[2] = sm._dyeScale[2];

   _dyeTranslation[0] = sm._dyeTranslation[0];
   _dyeTranslation[1] = sm._dyeTranslation[1];
   _dyeTranslation[2] = sm._dyeTranslation[2];

   _noiseScale[0] = sm._noiseScale[0];
   _noiseScale[1] = sm._noiseScale[1];
   _noiseScale[2] = sm._noiseScale[2];*/
}

/////////////////////////////////////////////////////////////
//Destructor                                               //
/////////////////////////////////////////////////////////////
cfdOSGAdvectionShaderManager::~cfdOSGAdvectionShaderManager()
{
   if(_noiseCbk){
      delete _noiseCbk;
      _noiseCbk = 0;
   }
   if(_noiseScaleCallback){
      delete _noiseScaleCallback;
      _noiseScaleCallback = 0;
   }
   if(_deltaCallback){
      delete _deltaCallback;
      _deltaCallback = 0;
   }
   if(_timeCallback){
      delete _timeCallback;
      _timeCallback = 0;
   }
   if(_periodCallback){
      delete _periodCallback;
      _periodCallback = 0;
   }
   if(_dyeTransCallback){
      delete _dyeTransCallback;
      _dyeTransCallback = 0;
   }
   if(_dyeScaleCallback){
      delete _dyeScaleCallback;
      _dyeScaleCallback = 0;
   }
   
}
/////////////////////////////////////////
void cfdOSGAdvectionShaderManager::Init()
{
   _initPropertyTexture();
   _initDyeTexture();
   _initNoiseTexture();
   _initLookUpFunction();
   _initWeightFunctions();
   _initFragProgramCallbacks();

   if(_velocity.valid()){
      _ss = new osg::StateSet();
      _ss->setTextureAttributeAndModes(0,_propertyToAdvect.get(), 
                                      osg::StateAttribute::OVERRIDE |osg::StateAttribute::ON);
      _ss->setTextureAttributeAndModes(1,_noiseTexture.get(), 
                                      osg::StateAttribute::OVERRIDE |osg::StateAttribute::ON);
      _ss->setTextureAttributeAndModes(2,_velocity.get(), 
                                      osg::StateAttribute::OVERRIDE |osg::StateAttribute::ON);
      _ss->setTextureAttributeAndModes(3,_dye.get(), 
                                     osg::StateAttribute::OVERRIDE |osg::StateAttribute::ON);
      _ss->setTextureAttributeAndModes(4,_lookUpFunction.get(), 
                                      osg::StateAttribute::OVERRIDE |osg::StateAttribute::ON);
      _ss->setTextureAttributeAndModes(5,_weightW.get(), 
                                      osg::StateAttribute::OVERRIDE |osg::StateAttribute::ON);
      _ss->setTextureAttributeAndModes(6,_weightV.get(), 
                                      osg::StateAttribute::OVERRIDE |osg::StateAttribute::ON);

      //load the shader file 
      char directory[1024];
      if(_shaderDirectory){
         strcpy(directory,_shaderDirectory);
      }else{
         char* vesuitehome = getenv("VE_SUITE_HOME");
         strcpy(directory,vesuitehome);
        strcat(directory,"/VE_Xplorer/cg_shaders/");
      }
      strcat(directory,"fragAdvect.cg");
      
      //set up our shader params
      _setupCGShaderProgram(_ss.get(),directory,"fp_advectTexture");
   }else{
      std::cout<<"Invalid velocity field!!"<<std::endl;
      std::cout<<"cfdOSGAdvectionShaderManager::Init()"<<std::endl;
   }
}
/////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::_initFragProgramCallbacks()
{
   _noiseScaleCallback = new cfdUpdateParameterCallback();
   _deltaCallback = new cfdUpdateParameterCallback();
   _timeCallback = new cfdUpdateParameterCallback();
   _periodCallback = new cfdUpdateParameterCallback();
   _dyeScaleCallback = new cfdUpdateParameterCallback();
   _dyeTransCallback = new cfdUpdateParameterCallback();

   _noiseScaleCallback->setType(2);
   _deltaCallback->setType(0);
   _timeCallback->setType(0);
   _periodCallback->setType(0);
   _dyeScaleCallback->setType(2);
   _dyeTransCallback->setType(2);
}
//////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::_setupCGShaderProgram(osg::StateSet* ss,
                                                         char* progName,
                                                         char* funcName)
{
    // create fragment program
    osgNVCg::Program *fprog = new osgNVCg::Program(osgNVCg::Program::FP30);
    fprog->setFileName(progName);
    fprog->setEntryPoint(funcName);
    fprog->setUseOptimalOptions(true);

    //set up the "updateable" parameters
    fprog->addVectorParameter("dyeTranslation")->setCallback(_dyeTransCallback);
    fprog->addVectorParameter("dyeScale")->setCallback(_dyeScaleCallback);
    fprog->addVectorParameter("texCoordMult")->setCallback(_noiseScaleCallback);
    fprog->addVectorParameter("deltaT")->setCallback(_deltaCallback);
    fprog->addVectorParameter("time")->setCallback(_timeCallback);
    fprog->addVectorParameter("period")->setCallback(_periodCallback);
    //apply the shaders to state set
     _ss->setAttributeAndModes(fprog,osg::StateAttribute::OVERRIDE);
}
////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateDeltaT(float deltaT)
{
   //_deltaT = deltaT;
   _deltaCallback->updateParameter(&deltaT); 
}
////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateNoiseScale(float* scale )
{
   /*_noiseScale[0] = scale[0];
   _noiseScale[1] = scale[1];
   _noiseScale[2] = scale[2];*/
   _noiseScaleCallback->updateParameter(scale);
}
////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateDyeScale(float* scale )
{
   /*_dyeScale[0] = scale[0];
   _dyeScale[1] = scale[1];
   _dyeScale[2] = scale[2];*/
   _dyeScaleCallback->updateParameter(scale);
}
////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateDyeTranslation(float* translation )
{
   /*_dyeTranslation[0] = translation[0];
   _dyeTranslation[1] = translation[1];
   _dyeTranslation[2] = translation[2];*/
   _dyeTransCallback->updateParameter(translation);
}
///////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::SetFieldSize(unsigned int x,
		                                     unsigned int y,
						                          unsigned int z)
{
   _fieldSize[0] = x;
   _fieldSize[1] = y;
   _fieldSize[2] = z;
}
///////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::SetVelocityTexture(osg::Texture3D* velocity)
{
   _velocity = velocity;
}
////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateInjectionPeriod(GLfloat period)
{
   //_period = period;
   _periodCallback->updateParameter(&period);
}
///////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateTime(GLfloat time)
{
   //_time = time;
   _timeCallback->updateParameter(&time);
}
///////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateNoiseFunction(float param,
                                        NoiseParam whichFunction)
{
   if(_noiseCbk){
      switch (whichFunction){
          case TAO_H:
             _noiseCbk->UpdateTaoH(param);
             break;
          case TAO_I:
             _noiseCbk->UpdateTaoI(param);
             break;
      };
   }
}
//////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateWeight(GLfloat param,
                                        int whichMaterial)
{
   //do nothing for now
}
//////////////////////////////////////////////////////////////////
osg::Texture3D* cfdOSGAdvectionShaderManager::GetPropertyTexture()
{
   if(_propertyToAdvect.valid()){
      return _propertyToAdvect.get();
   }
   return 0;
}
/////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::_initPropertyTexture()
{
   if(_fieldSize[0] &&
      _fieldSize[1] &&
      _fieldSize[2]){ 
      int dataSize = _fieldSize[0]*_fieldSize[1]*_fieldSize[2];
      unsigned char* data = new unsigned char[dataSize*4];
      unsigned int i=0;
      unsigned int j=0;
      unsigned int k = 0;
      for(int p = 0; p < dataSize; p++){
          if((i == 0 || i == _fieldSize[0] - 1)||
           (j == 0 || j == _fieldSize[1] - 1)||
           (k == 0 || k == _fieldSize[2] - 1)){
            data[p*4   ] = (unsigned char)0;
            data[p*4 + 1] = (unsigned char)0;
            data[p*4 + 2] = (unsigned char)0;
            data[p*4 + 3] = (unsigned char)0; 
        }else{
           data[p*4   ] = (unsigned char)0;
           data[p*4 + 1] = (unsigned char)p%255;
           data[p*4 + 2] = (unsigned char)0;
           data[p*4 + 3] = (unsigned char)127;   
        }
        i++;
        if((unsigned int)i > (unsigned int)_fieldSize[0]-1){
            i = 0;
            j ++;
           if((unsigned int)j > (unsigned int)_fieldSize[1]-1){
               j = 0;
               k ++;
               if((unsigned int)k > (unsigned int)_fieldSize[2]-1){
                  k = 0;
               }
           }
        }
      }
      /*for(int p = 0; p < dataSize; p++){
      
         data[p*4   ] = (unsigned char)0;

         data[p*4 + 1] = (unsigned char)0;
         data[p*4 + 2] = (unsigned char)0;
     
         data[p*4 + 3] = (unsigned char)0;      
      }*/
      osg::ref_ptr<osg::Image> propertyField = new osg::Image();

      propertyField->allocateImage(_fieldSize[0],
                                _fieldSize[1],
                                _fieldSize[2],
                                GL_RGBA,GL_UNSIGNED_BYTE);

      propertyField->setImage(_fieldSize[0], _fieldSize[1], _fieldSize[2],
		                      GL_RGBA,
		                      GL_RGBA,
			                   GL_UNSIGNED_BYTE,
                           data,/*may need a function to init empty data*/
                           osg::Image::USE_NEW_DELETE,1);
      
      propertyField->setDataVariance(osg::Object::DYNAMIC);

      _propertyToAdvect = new osg::Texture3D();
      _propertyToAdvect->setDataVariance(osg::Object::DYNAMIC);
      _propertyToAdvect->setFilter(osg::Texture3D::MIN_FILTER,osg::Texture3D::LINEAR);
      _propertyToAdvect->setFilter(osg::Texture3D::MAG_FILTER,osg::Texture3D::LINEAR);
      _propertyToAdvect->setWrap(osg::Texture3D::WRAP_R,osg::Texture3D::CLAMP_TO_EDGE);
      _propertyToAdvect->setWrap(osg::Texture3D::WRAP_S,osg::Texture3D::CLAMP_TO_EDGE);
      _propertyToAdvect->setWrap(osg::Texture3D::WRAP_T,osg::Texture3D::CLAMP_TO_EDGE);
      _propertyToAdvect->setInternalFormat(GL_RGBA);
      _propertyToAdvect->setTextureSize(_fieldSize[0],
                                _fieldSize[1],
                                _fieldSize[2]);
      _propertyToAdvect->setImage(propertyField.get());
      //_propertyToAdvect->setSubloadCallback(new cfdCopyTextureCallback());
      /*if(data){
         delete [] data;
         data = 0;
      }*/
   }else{
      std::cout<<"Invalid field size!!"<<std::endl;
      std::cout<<"cfdOSGTransferShaderManager::_initPropertyTexture"<<std::endl;
   }

}
////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::_initDyeTexture()
{
   unsigned char* dyeTex = 0;
   dyeTex = new unsigned char[4*4*4];
   int index = 0;
   //dye emitter amplitude texture
   for ( int i = 0; i < 4; i++) {
      for(int j = 0; j < 4; j++){
         for(int k = 0; k < 4; k++){
            if(i ==0||i == 3
               ||j == 0 || j == 3
               ||k ==0 || k== 3){
               dyeTex[index] = (GLubyte)0;
            }else{
               dyeTex[index    ] = (GLubyte)255;
            }
            index++;
	 }
      }
   }
   osg::ref_ptr<osg::Image> dyeImage = new osg::Image();
   dyeImage->allocateImage(4, 4,4,
                        GL_LUMINANCE,GL_UNSIGNED_BYTE);

   dyeImage->setImage(4, 4, 4,
		      GL_LUMINANCE,
		      GL_LUMINANCE,
		      GL_UNSIGNED_BYTE,
		      dyeTex,
		      osg::Image::USE_NEW_DELETE,1);
   dyeImage->setDataVariance(osg::Object::DYNAMIC);
   if(!_dye.valid()){
      _dye= new osg::Texture3D();
      _dye->setFilter(osg::Texture3D::MIN_FILTER,
                          osg::Texture3D::LINEAR);
      _dye->setFilter(osg::Texture3D::MAG_FILTER,
                          osg::Texture3D::LINEAR);
      _dye->setWrap(osg::Texture3D::WRAP_S,
                         osg::Texture3D::CLAMP);
      _dye->setWrap(osg::Texture3D::WRAP_T,
                          osg::Texture3D::CLAMP);
      _dye->setWrap(osg::Texture3D::WRAP_R,
                           osg::Texture3D::CLAMP);

      _dye->setTextureSize(4,4,4);
      _dye->setInternalFormat(GL_LUMINANCE);
      _dye->setDataVariance(osg::Object::DYNAMIC);
   }
   _dye->setImage(dyeImage.get());
   if(dyeTex){
      delete [] dyeTex;
      dyeTex = 0;
   }
}
//////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::_initNoiseTexture()
{
   if(_noiseCbk)
      return;
   GLuint hI[256];
   GLuint ga[256];
   GLuint gI[256];
   GLfloat taoAlpha = .1;
   GLfloat taoI  = .1;
   GLfloat taoH = .9;
  //build ga
  for(unsigned int i = 0; i < 256; i++){
     if(i < taoAlpha*255){
        ga[i] = 0;
     }else{
        ga[i] = 255.0*(255 - i/*taoAlpha*255*/)/(255.0-taoAlpha*255);
     }
  }

  for(unsigned int i = 0; i < 256; i++){
     if(i < taoI*255){
        gI[i] = 0;
     }else{
        gI[i] = 255;
     }
  }

  //build hI transfer
  for(unsigned int i = 0; i < 256; i++){
     if(i < taoH*255){
        hI[i] = 0;
     }else{
        hI[i] = i;
     }
  }
  
  unsigned int phase[32][32][32];
   for(unsigned int i = 0; i <32; i++)
      for(unsigned int j = 0; j <32; j++)
         for(unsigned int k = 0; k <32; k++)
            phase[i][j][k] = rand()%256;

   unsigned char noiseTex[32*32*32*4] ;
   unsigned int nData = 32*32*32;
   unsigned int t = 0;
   unsigned int index = 0;
   //for (int i = 0; i < nData; i++) {
    for(int i = 0; i < 32; i++){
      t = i*256/32;
      for(unsigned int j = 0; j < 32; j++){
         for(unsigned int k = 0; k < 32; k++){
      noiseTex[index*4    ] = (unsigned char)(GLubyte)((hI[(phase[i][j][k]+t) % 255])*
                                           (gI[(phase[k][j][i]+t) % 255]));
      noiseTex[index*4 + 1] = (unsigned char)(GLubyte)phase[i][j][k];
      noiseTex[index*4  +2] = (unsigned char)(gI[(phase[i][j][k] + t) % 255])*
                                           (hI[(phase[k][j][i] + t) % 255]);
      noiseTex[index*4  +3] = (unsigned char) (GLubyte)phase[k][j][i];
      index++;
         }
      }
    }
   
   _noiseTexture = new osg::Texture3D();
   _noiseTexture->setDataVariance(osg::Object::DYNAMIC);
   _noiseTexture->setFilter(osg::Texture3D::MIN_FILTER,
                                   osg::Texture3D::NEAREST);
   _noiseTexture->setFilter(osg::Texture3D::MAG_FILTER,
                                   osg::Texture3D::NEAREST);
   _noiseTexture->setWrap(osg::Texture3D::WRAP_S,
                          osg::Texture3D::REPEAT);
   _noiseTexture->setWrap(osg::Texture3D::WRAP_T,
                          osg::Texture3D::REPEAT);
   _noiseTexture->setWrap(osg::Texture3D::WRAP_R,
                           osg::Texture3D::REPEAT);
   _noiseTexture->setTextureSize(32,32,32);
   _noiseTexture->setInternalFormat(GL_RGBA);

   osg::ref_ptr<osg::Image> noiseyImage = new osg::Image();
   noiseyImage->setImage(32,32,32,
		                 GL_RGBA,
			              GL_RGBA,
			              GL_UNSIGNED_BYTE,
   			           noiseTex,
   			           osg::Image::USE_NEW_DELETE,1);
   noiseyImage->setDataVariance(osg::Object::DYNAMIC);

   _noiseTexture->setImage(noiseyImage.get());
   _noiseTexture->setTextureSize(32,32,32);
  
   _noiseCbk = new cfdUpdateableOSGNoiseTexture3d();
   _noiseTexture->setSubloadCallback(_noiseCbk);
}
/////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::_initWeightFunctions()
{
   //this does nothing for now. . .can be functions that are
   //weighted based on velocity
   osg::ref_ptr<osg::Image> tempW = new osg::Image();
   osg::ref_ptr<osg::Image> tempV = new osg::Image();

   tempW->allocateImage(2,2,2,GL_RGBA,GL_UNSIGNED_BYTE);
   tempW->setImage(2,2,2,GL_RGBA, GL_RGBA, GL_UNSIGNED_BYTE,
                           0,
                           osg::Image::USE_NEW_DELETE,1);
   tempW->setDataVariance(osg::Object::DYNAMIC);

   tempV->allocateImage(2,2,2,GL_RGBA,GL_UNSIGNED_BYTE);
   tempV->setImage(2,2,2,GL_RGBA, GL_RGBA, GL_UNSIGNED_BYTE,
                           0,
                           osg::Image::USE_NEW_DELETE,1);
   tempV->setDataVariance(osg::Object::DYNAMIC);

   _weightW = new osg::Texture3D();
   _weightW->setDataVariance(osg::Object::DYNAMIC);
   _weightW->setFilter(osg::Texture3D::MIN_FILTER,
                                   osg::Texture3D::LINEAR);
   _weightW->setFilter(osg::Texture3D::MAG_FILTER,
                                   osg::Texture3D::LINEAR);
   _weightW->setWrap(osg::Texture3D::WRAP_S,
                          osg::Texture3D::REPEAT);
   _weightW->setWrap(osg::Texture3D::WRAP_T,
                          osg::Texture3D::REPEAT);
   _weightW->setWrap(osg::Texture3D::WRAP_R,
                           osg::Texture3D::REPEAT);
   _weightW->setInternalFormat(GL_RGBA);
   _weightW->setImage(tempW.get());

   _weightV = new osg::Texture3D();
   _weightV->setDataVariance(osg::Object::DYNAMIC);
   _weightV->setFilter(osg::Texture3D::MIN_FILTER,
                                   osg::Texture3D::LINEAR);
   _weightV->setFilter(osg::Texture3D::MAG_FILTER,
                                   osg::Texture3D::LINEAR);
   _weightV->setWrap(osg::Texture3D::WRAP_S,
                          osg::Texture3D::REPEAT);
   _weightV->setWrap(osg::Texture3D::WRAP_T,
                          osg::Texture3D::REPEAT);
   _weightV->setWrap(osg::Texture3D::WRAP_R,
                           osg::Texture3D::REPEAT);
   _weightV->setInternalFormat(GL_RGBA);
   _weightV->setImage(tempV.get());
}
////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::_initLookUpFunction()
{
   
   GLfloat theta = 0;
   GLfloat value = 0;
   unsigned char lutex[256];
   for(int i = 0; i < 256; i++){
      theta = i*(PI/255.0);
       value = sin(theta); 
       lutex[i] = (GLubyte)(255.0*value);
   } 

   _lookUpFunction = new osg::Texture1D();
   osg::ref_ptr<osg::Image> data = new osg::Image();
   data->allocateImage(256,1,1,GL_LUMINANCE,GL_UNSIGNED_BYTE);
   data->setImage(256,1,1,GL_LUMINANCE,GL_LUMINANCE,GL_UNSIGNED_BYTE,lutex,
                osg::Image::USE_NEW_DELETE);
   data->setDataVariance(osg::Object::DYNAMIC);
   
   osg::ref_ptr<osg::Texture1D>trans = new osg::Texture1D;
   trans->setDataVariance(osg::Object::DYNAMIC);
   _lookUpFunction->setTextureSize(256);
   _lookUpFunction->setFilter(osg::Texture1D::MIN_FILTER,
                    osg::Texture1D::LINEAR);
   _lookUpFunction->setFilter(osg::Texture1D::MAG_FILTER,
                    osg::Texture1D::LINEAR);
   _lookUpFunction->setWrap(osg::Texture1D::WRAP_S,
                  osg::Texture3D::CLAMP);
   _lookUpFunction->setInternalFormat(GL_LUMINANCE);
   _lookUpFunction->setDataVariance(osg::Object::DYNAMIC);
   _lookUpFunction->setImage(data.get());
}
///////////////////////////////////////////////////////////////////////////
cfdOSGAdvectionShaderManager& cfdOSGAdvectionShaderManager::operator=(const 
                                     cfdOSGAdvectionShaderManager& sm)
{
   if(this != &sm){
      _velocity = sm._velocity;
      _propertyToAdvect = sm._velocity;
      _weightW = sm._weightW;
      _weightV = sm._weightV;
      _lookUpFunction = sm._lookUpFunction;
      _reinit = sm._reinit;
      _dye = sm._dye;
      if(_noiseCbk){
         delete _noiseCbk;
	       _noiseCbk = 0;
      }
      _noiseCbk = new cfdUpdateableOSGNoiseTexture3d(*sm._noiseCbk);
      
      _fieldSize[0] = sm._fieldSize[0];
      _fieldSize[1] = sm._fieldSize[1];
      _fieldSize[2] = sm._fieldSize[2];

      if(_noiseScaleCallback){
         delete _noiseScaleCallback;
         _noiseScaleCallback = 0;
      }
      if(_deltaCallback){
         delete _deltaCallback;
         _deltaCallback = 0;
      }
      if(_timeCallback){
         delete _timeCallback;
         _timeCallback = 0;
      }
      if(_periodCallback){
         delete _periodCallback;
         _periodCallback = 0;
      }
      if(_dyeTransCallback){
         delete _dyeTransCallback;
         _dyeTransCallback = 0;
      }
      if(_dyeScaleCallback){
         delete _dyeScaleCallback;
         _dyeScaleCallback = 0;
      }
      _noiseScaleCallback = new cfdUpdateParameterCallback(*sm._noiseScaleCallback);
      _deltaCallback = new cfdUpdateParameterCallback(*sm._deltaCallback);
      _timeCallback = new cfdUpdateParameterCallback(*sm._timeCallback);
      _periodCallback = new cfdUpdateParameterCallback(*sm._periodCallback);
      _dyeScaleCallback = new cfdUpdateParameterCallback(*sm._dyeScaleCallback);
      _dyeTransCallback = new cfdUpdateParameterCallback(*sm._dyeTransCallback);
      /*_dyeScale[0] = sm._dyeScale[0];
      _dyeScale[1] = sm._dyeScale[1];
      _dyeScale[2] = sm._dyeScale[2];

      _dyeTranslation[0] = sm._dyeTranslation[0];
      _dyeTranslation[1] = sm._dyeTranslation[1];
      _dyeTranslation[2] = sm._dyeTranslation[2];

      _noiseScale[0] = sm._noiseScale[0];
      _noiseScale[1] = sm._noiseScale[1];
      _noiseScale[2] = sm._noiseScale[2];*/
   }
   return *this;
}
#endif// _CFD_USE_SHADERS
#endif//_OSG
