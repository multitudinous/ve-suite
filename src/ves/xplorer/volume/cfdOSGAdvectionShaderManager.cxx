/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/volume/cfdOSGAdvectionShaderManager.h>
#include <ves/xplorer/volume/cfdUpdateableOSGTexture1d.h>
#include <ves/xplorer/volume/cfdSimpleTextureCallback.h>
#include <ves/xplorer/volume/cfdUpdateParameterCallback.h>
#include <ves/xplorer/volume/cfdUpdateableOSGNoiseTexture3d.h>

// --- OSG Includes --- //
#include <osg/Array>
#include <osg/Texture3D>
#include <osg/Texture1D>
#include <osg/TexEnv>
#include <osg/BlendFunc>

// --- C/C++ Includes --- //
#include <vector>
#include <iostream>
#include <string>
#include <cstdlib>

using namespace ves::xplorer::volume;

#define PI  3.1416

//the shader inline source
static const char* advectionFragSource =
    {
        "uniform sampler3D noiseTexture;\n"
        "uniform sampler3D velocity;\n"
        "uniform sampler3D dye;\n"
        "uniform sampler1D lookUpTexture;\n"
        "uniform sampler3D property;\n"
        "uniform vec3 dyeTranslation;\n"
        "uniform vec3 dyeScale;\n"
        "uniform vec3 texCoordMult;\n"
        "uniform vec3 deltaT;\n"
        "uniform float time;\n"
        "uniform float period;\n"
        "uniform vec3 weightW;\n"
        "uniform vec3 weightV;\n"

        "void main(void)\n"
        "{\n"

        "//look up the velocity in the field\n"
        "vec4 v = texture3D(velocity,gl_TexCoord[0].xyz);\n"

        "//get our original values back\n"
        "v.xyz = (((v.xyz)*2.0) - 1.0);\n"

        "//velocity mask to darken slow moving flow\n"
        "float vMask = 1.0 - v.w;\n"

        "//Euler integration\n"
        "//the old position\n"
        "vec3 oldTexCoord = gl_TexCoord[0].xyz + deltaT*v.xyz;\n"

        "//fetch the density using the old coord\n"
        "//density is our property that we are \n"
        "//advecting\n"
        "vec4 prop = texture3D(property,oldTexCoord);\n"

        "//now for our materials\n"

        "//lookup the noise amplitude/phase\n"
        "vec4 n = texture3D(noiseTexture,gl_TexCoord[0].xyz*texCoordMult);\n"

        "//dye coordinate\n"
        "vec3 relFragCoord = (gl_TexCoord[0].xyz-dyeTranslation)*dyeScale; \n"
        "float dyeAmp = texture3D(dye,relFragCoord).a;\n"

        "//now do the alpha blending for each material\n"

        "//material 1\n"
        "//get the local time\n"
        "gl_FragColor.x = weightW.x*prop.x + dyeAmp;\n"

        "//material 1\n"
        "float localTime = mod(time + n.y,period);\n"
        "float tInject = texture1D(lookUpTexture,localTime).a;\n"
        "gl_FragColor.y = weightW.y*prop.y + weightV.y*tInject*n.x;\n"

        "//material 2\n"
        "localTime = mod(time + n.w,period);\n"
        "tInject = texture1D(lookUpTexture,localTime).a;\n"
        "gl_FragColor.z = weightW.z*prop.z + weightV.z*tInject*n.z;\n"
        "gl_FragColor.a = v.w;\n"
        "}\n"
    };

////////////////////////////////////////////////////////////////////////////////
cfdOSGAdvectionShaderManager::cfdOSGAdvectionShaderManager()
        : cfdOSGShaderManager()
{
    //_deltaT = 1;
    _reinit = true;
//   _period = 1;
    // _time = 0;
    _isFrag =  true;
    _noiseScaleCallback = 0 ;
    _dyeCoordCallback = 0;
    _deltaCallback = 0 ;
    _timeCallback = 0 ;
    _periodCallback = 0;
    _dyeScaleCallback = 0 ;
    _dyeTransCallback = 0;
    //_noiseCbk = 0;

    _weightWCallback = 0;
    _weightVCallback = 0;

    _fieldSize[0] = 0;
    _fieldSize[1] = 0;
    _fieldSize[2] = 0;

    _weightV[0] = .8;
    _weightV[1] = .8;
    _weightV[2] = .8;
    _weightV[3] = .8;

    _weightW[0] = .2;
    _weightW[1] = .2;
    _weightW[2] = .2;
    _weightW[3] = .2;

    _center.set( 0, 0, 0 );
}
////////////////////////////////////////////////////////////////////////////////
cfdOSGAdvectionShaderManager::cfdOSGAdvectionShaderManager( const
                                                            cfdOSGAdvectionShaderManager& sm )
        : cfdOSGShaderManager( sm )
{
    _center = sm._center;
    //_period = sm._period;
    //_time = sm._time;
    //only want a pointer here
    _velocity = sm._velocity;
    _isFrag = sm._isFrag;
    //new these
    _propertyToAdvect = new osg::Texture3D( *( sm._propertyToAdvect.get() ) );

    _lookUpFunction = new osg::Texture1D( *( sm._lookUpFunction.get() ) );
    _noiseCbk = new cfdUpdateableOSGNoiseTexture3d( *sm._noiseCbk );
    _dye = new osg::Texture3D( *( sm._dye.get() ) );
    _reinit = sm._reinit;
    //_deltaT= sm._deltaT;
    _fieldSize[0] = sm._fieldSize[0];
    _fieldSize[1] = sm._fieldSize[1];
    _fieldSize[2] = sm._fieldSize[2];

    _noiseScaleCallback = new cfdUpdateParameterCallback( *sm._noiseScaleCallback );
    _deltaCallback = new cfdUpdateParameterCallback( *sm._deltaCallback );
    _timeCallback = new cfdUpdateParameterCallback( *sm._timeCallback );
    _periodCallback = new cfdUpdateParameterCallback( *sm._periodCallback );
    _weightWCallback = new cfdUpdateParameterCallback( *sm._weightWCallback );
    _weightVCallback = new cfdUpdateParameterCallback( *sm._weightVCallback );


    _weightV[0] = sm._weightV[0];
    _weightV[1] = sm._weightV[1];
    _weightV[2] = sm._weightV[2];
    _weightV[3] = sm._weightV[3];

    _weightW[0] = sm._weightW[0];
    _weightW[1] = sm._weightW[1];
    _weightW[2] = sm._weightW[2];
    _weightW[3] = sm._weightW[3];
}
////////////////////////////////////////////////////////////////////////////////
cfdOSGAdvectionShaderManager::~cfdOSGAdvectionShaderManager()
{
    /*if(_noiseCbk){
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
    if(_dyeScaleCallback){
       delete _dyeScaleCallback;
       _dyeScaleCallback = 0;
    }
    if(_dyeTransCallback)
    {
       delete _dyeTransCallback;
       _dyeTransCallback = 0;
    }

     if(_weightWCallback)
     {
        delete _weightWCallback;
        _weightWCallback = 0;
     }
     if(_weightVCallback)
     {
        delete _weightWCallback;
        _weightWCallback = 0;
     }
    if(_dyeMatCallback){
       delete _dyeMatCallback;
       _dyeMatCallback = 0;
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::Init()
{
    _initNoiseTexture();
    _initDyeTexture();
    _initLookUpFunction();
    _initPropertyTexture();
    _initWeightFunctions();
    _initFragProgramCallbacks();

    if( _velocity.valid() )
    {
        //set up the state for textures
        _ss = new osg::StateSet();
        _ss->setMode( GL_ALPHA_TEST, osg::StateAttribute::ON );
        _ss->setMode( GL_LIGHTING, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setMode( GL_DEPTH_TEST, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setMode( GL_SCISSOR_TEST, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setTextureAttributeAndModes( 0, _noiseTexture.get(),
                                          osg::StateAttribute::OVERRIDE | osg::StateAttribute::ON );
        _ss->setTextureMode( 0, GL_TEXTURE_3D, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setTextureMode( 0, GL_TEXTURE_GEN_S, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setTextureMode( 0, GL_TEXTURE_GEN_T, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setTextureMode( 0, GL_TEXTURE_GEN_R, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );

        _ss->setTextureAttributeAndModes( 1, _velocity.get(),
                                          osg::StateAttribute::OVERRIDE | osg::StateAttribute::ON );
        _ss->setTextureMode( 1, GL_TEXTURE_3D, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setTextureMode( 1, GL_TEXTURE_GEN_S, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setTextureMode( 1, GL_TEXTURE_GEN_T, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setTextureMode( 1, GL_TEXTURE_GEN_R, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );

        _ss->setTextureAttributeAndModes( 2, _dye.get(),
                                          osg::StateAttribute::OVERRIDE | osg::StateAttribute::ON );
        _ss->setTextureMode( 2, GL_TEXTURE_3D, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setTextureMode( 2, GL_TEXTURE_GEN_S, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setTextureMode( 2, GL_TEXTURE_GEN_T, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setTextureMode( 2, GL_TEXTURE_GEN_R, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );

        _ss->setTextureAttributeAndModes( 3, _lookUpFunction.get(),
                                          osg::StateAttribute::OVERRIDE | osg::StateAttribute::ON );
        _ss->setTextureMode( 3, GL_TEXTURE_1D, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setTextureMode( 3, GL_TEXTURE_GEN_S, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setTextureMode( 3, GL_TEXTURE_GEN_T, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _ss->setTextureMode( 3, GL_TEXTURE_GEN_R, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
        _tUnit = 4;
        _setupStateSetForGLSL();
    }
    else
    {
        std::cout << "Invalid velocity field!!" << std::endl;
        std::cout << "cfdOSGAdvectionShaderManager::Init()" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::_setupStateSetForGLSL()
{
    std::cout << "Using glsl..." << std::endl;

    osg::ref_ptr<osg::Uniform> dyeTrans = new osg::Uniform( "dyeTranslation", osg::Vec3f( 0.0f, 0.0f, 0.0f ) );

    osg::ref_ptr<osg::Uniform> dyeScale = new osg::Uniform( "dyeScale", osg::Vec3f( .5*_fieldSize[0] / 4.0,
                                                            .5*_fieldSize[1] / 4.0,
                                                            .5*_fieldSize[2] / 4.0 ) );

    osg::ref_ptr<osg::Uniform> tCoordMult = new osg::Uniform( "texCoordMult", osg::Vec3f( .5*_fieldSize[0] / 32.0,
                                                              .5*_fieldSize[1] / 32.0,
                                                              .5*_fieldSize[2] / 32.0 ) );

    osg::ref_ptr<osg::Uniform> dT = new osg::Uniform( "deltaT", osg::Vec3f( 1.0 / _fieldSize[0],
                                                      1.0 / _fieldSize[1],
                                                      1.0 / _fieldSize[2] ) );

    osg::ref_ptr<osg::Uniform> t = new osg::Uniform( "time", 0.0f );
    osg::ref_ptr<osg::Uniform> period = new osg::Uniform( "period", 1.0f );
    osg::ref_ptr<osg::Uniform> wW = new osg::Uniform( "weightW", osg::Vec3f( .2, .2, .2 ) );
    osg::ref_ptr<osg::Uniform> wV = new osg::Uniform( "weightV", osg::Vec3f( .8, .8, .8 ) );

    osg::ref_ptr<osg::Shader> vTransfers = _createGLSLShaderFromInline( advectionFragSource, true );


    osg::ref_ptr<osg::Program> glslProgram = new osg::Program();

    glslProgram->addShader( vTransfers.get() );
    AddShaderProgram( "3D Texture Advection", glslProgram );
    /*
    _setupGLSLShaderProgram(_ss.get(),glslProgram.get(),
                         std::string("fragAdvect"),true);*/
    _ss->addUniform( dyeTrans.get() );
    _ss->addUniform( dyeScale.get() );
    _ss->addUniform( tCoordMult.get() );
    _ss->addUniform( dT.get() );
    _ss->addUniform( t.get() );
    _ss->addUniform( period.get() );
    _ss->addUniform( wW.get() );
    _ss->addUniform( wV.get() );

    dyeTrans->setUpdateCallback( _dyeTransCallback.get() );
    dyeScale->setUpdateCallback( _dyeScaleCallback.get() );
    tCoordMult->setUpdateCallback( _noiseScaleCallback.get() );
    t->setUpdateCallback( _timeCallback.get() );
    dT->setUpdateCallback( _deltaCallback.get() );
    wV->setUpdateCallback( _weightVCallback.get() );
    wW->setUpdateCallback( _weightWCallback.get() );
    period->setUpdateCallback( _periodCallback.get() );


    _ss->addUniform( new osg::Uniform( "noiseTexture", 0 ) );
    _ss->addUniform( new osg::Uniform( "velocity", 1 ) );
    _ss->addUniform( new osg::Uniform( "dye", 2 ) );
    _ss->addUniform( new osg::Uniform( "lookUpTexture", 3 ) );
    _ss->addUniform( new osg::Uniform( "property", 4 ) );
    /*if(fullPath)
    {
       delete [] fullPath;
       fullPath = 0;
    }*/

}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::_initFragProgramCallbacks()
{
    if( _noiseScaleCallback.valid() )
        return;
    _noiseScaleCallback = new cfdUpdateParameterCallback();
    _deltaCallback = new cfdUpdateParameterCallback();
    _timeCallback = new cfdUpdateParameterCallback();
    _periodCallback = new cfdUpdateParameterCallback();
    _dyeTransCallback = new cfdUpdateParameterCallback();
    _dyeScaleCallback = new cfdUpdateParameterCallback();
    _weightWCallback = new cfdUpdateParameterCallback();
    _weightVCallback = new cfdUpdateParameterCallback();

    float period = 1.0;
    float delta[3];
    delta[0] = 1.0 / _fieldSize[0];
    delta[1] = 1.0 / _fieldSize[1];
    delta[2] = 1.0 / _fieldSize[2];

    float dyeScale[3] = {1, 1, 1};
    float dyeTrans[3] = {.05, .41, .449};
    float noiseScale[3] = {.5, .5, .5};

    noiseScale[0] = .5 * _fieldSize[0] / 32.0;
    noiseScale[1] = .5 * _fieldSize[1] / 32.0;
    noiseScale[2] = .5 * _fieldSize[2] / 32.0;

    dyeScale[0] = .5 * _fieldSize[0] / 4.0;
    dyeScale[1] = .5 * _fieldSize[1] / 4.0;
    dyeScale[2] = .5 * _fieldSize[2] / 4.0;

    _dyeScaleCallback->setTypeAndSize( cfdUpdateParameterCallback::VECTOR,
                                       cfdUpdateParameterCallback::THREE );
    _dyeScaleCallback->updateParameter( dyeScale );

    _dyeTransCallback->setTypeAndSize( cfdUpdateParameterCallback::VECTOR,
                                       cfdUpdateParameterCallback::THREE );
    _dyeTransCallback->updateParameter( dyeTrans );

    _noiseScaleCallback->setTypeAndSize( cfdUpdateParameterCallback::VECTOR,
                                         cfdUpdateParameterCallback::THREE );
    _noiseScaleCallback->updateParameter( noiseScale );

    _deltaCallback->setTypeAndSize( cfdUpdateParameterCallback::VECTOR,
                                    cfdUpdateParameterCallback::THREE );
    _deltaCallback->updateParameter( delta );

    _periodCallback->setTypeAndSize( cfdUpdateParameterCallback::VECTOR,
                                     cfdUpdateParameterCallback::ONE );
    _periodCallback->updateParameter( &period );

    _weightWCallback->setTypeAndSize( cfdUpdateParameterCallback::VECTOR,
                                      cfdUpdateParameterCallback::THREE );

    _weightVCallback->setTypeAndSize( cfdUpdateParameterCallback::VECTOR,
                                      cfdUpdateParameterCallback::THREE );
    _timeCallback->setTypeAndSize( cfdUpdateParameterCallback::TIME,
                                   cfdUpdateParameterCallback::ONE );
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::SetCenter( osg::Vec3 center )
{
    _center = center;
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateDeltaT( float deltaT )
{
    //_deltaT = deltaT;
    _deltaCallback->updateParameter( &deltaT );
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateNoiseScale( float* scale )
{
    _noiseScaleCallback->updateParameter( scale );
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateDyeScale( float* scale )
{
    _dyeScaleCallback->updateParameter( scale );
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateDyeTranslation( float* translation )
{
    _dyeTransCallback->updateParameter( translation );
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::SetFieldSize( unsigned int x,
                                                 unsigned int y,
                                                 unsigned int z )
{
    _fieldSize[0] = x;
    _fieldSize[1] = y;
    _fieldSize[2] = z;
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::SetVelocityTexture( osg::Texture3D* velocity )
{
    _velocity = velocity;
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateInjectionPeriod( GLfloat period )
{
    period = 1.0;
    _periodCallback->updateParameter( &period );
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateTime( GLfloat time )
{
    _timeCallback->updateParameter( &time );
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateNoiseFunction( float param,
                                                        NoiseParam whichFunction )
{
    if( _noiseCbk.valid() )
    {
        switch ( whichFunction )
        {
            case TAO_H:
                _noiseCbk->UpdateTaoH( param );
                break;
            case TAO_I:
                _noiseCbk->UpdateTaoI( param );
                break;
        };
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::UpdateWeight( GLfloat* param,
                                                 int whichMaterial )
{
    switch ( whichMaterial )
    {
            //red -- or dye
        case 0:
            //decay
            _weightW[0] = 1.0 - param[0];
            //inject
            _weightV[0] = param[1];
            break;
            //green -- or material 1
        case 1:
            //decay
            _weightW[1] = 1.0 - param[0];
            //inject
            _weightV[1] = param[1];
            break;
            //blue -- or material 2
        case 2:
            //decay
            _weightW[2] = 1.0 - param[0];
            //inject
            _weightV[2] = param[1];
            break;
    }
    _weightWCallback->updateParameter( _weightW );
    _weightVCallback->updateParameter( _weightV );
}
////////////////////////////////////////////////////////////////////////////////
osg::Texture3D* cfdOSGAdvectionShaderManager::GetPropertyTexture()
{
    if( _propertyToAdvect.valid() )
    {
        return _propertyToAdvect.get();
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::_initPropertyTexture()
{
    if( _propertyToAdvect.valid() )
        return;
    if( _fieldSize[0] &&
            _fieldSize[1] &&
            _fieldSize[2] )
    {
        int dataSize = ( _fieldSize[0] ) * ( _fieldSize[1] ) * ( _fieldSize[2] );
        unsigned char* data = new unsigned char[dataSize*4];
        for( int p = 0; p < dataSize; p++ )
        {
            data[p*4   ] = ( unsigned char )0;
            data[p*4 + 1] = ( unsigned char )0;
            data[p*4 + 2] = ( unsigned char )0;
            data[p*4 + 3] = ( unsigned char )0;
        }
        osg::ref_ptr<osg::Image> propertyField = new osg::Image();

        propertyField->allocateImage( _fieldSize[0],
                                      _fieldSize[1],
                                      _fieldSize[2],
                                      GL_RGBA, GL_UNSIGNED_BYTE );

        propertyField->setImage( _fieldSize[0], _fieldSize[1], _fieldSize[2],
                                 GL_RGBA,
                                 GL_RGBA,
                                 GL_UNSIGNED_BYTE,
                                 data,/*may need a function to init empty data*/
                                 osg::Image::USE_NEW_DELETE, 1 );

        propertyField->setDataVariance( osg::Object::DYNAMIC );

        _propertyToAdvect = new osg::Texture3D();
        _propertyToAdvect->setDataVariance( osg::Object::DYNAMIC );
        _propertyToAdvect->setFilter( osg::Texture3D::MIN_FILTER, osg::Texture3D::LINEAR );
        _propertyToAdvect->setFilter( osg::Texture3D::MAG_FILTER, osg::Texture3D::LINEAR );
        _propertyToAdvect->setWrap( osg::Texture3D::WRAP_R, osg::Texture3D::CLAMP_TO_EDGE );
        _propertyToAdvect->setWrap( osg::Texture3D::WRAP_S, osg::Texture3D::CLAMP_TO_EDGE );
        _propertyToAdvect->setWrap( osg::Texture3D::WRAP_T, osg::Texture3D::CLAMP_TO_EDGE );
        _propertyToAdvect->setInternalFormat( GL_RGBA );
        _propertyToAdvect->setTextureSize( _fieldSize[0],
                                           _fieldSize[1],
                                           _fieldSize[2] );
        _propertyToAdvect->setImage( propertyField.get() );
        osg::ref_ptr<cfdSimpleTextureCallback> sCallback = new cfdSimpleTextureCallback();
        sCallback->setTextureSize( _fieldSize[0],
                                   _fieldSize[1],
                                   _fieldSize[2] );
        _propertyToAdvect->setSubloadCallback( sCallback.get() );

    }
    else
    {
        std::cout << "Invalid field size!!" << std::endl;
        std::cout << "cfdOSGTransferShaderManager::_initPropertyTexture" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::_initDyeTexture()
{
    if( _dye.valid() )
        return;
    unsigned char* dyeTex = 0;
    dyeTex = new unsigned char[4*4*4];
    int index = 0;
    //dye emitter amplitude texture
    for( int i = 0; i < 4; i++ )
    {
        for( int j = 0; j < 4; j++ )
        {
            for( int k = 0; k < 4; k++ )
            {
                if( i == 0 || i == 3
                        || j == 0 || j == 3
                        || k == 0 || k == 3 )
                {
                    dyeTex[index] = ( GLubyte )0;
                }
                else
                {
                    dyeTex[index] = ( GLubyte )255;
                }
                index++;
            }
        }
    }
    osg::ref_ptr<osg::Image> dyeImage = new osg::Image();
    dyeImage->allocateImage( 4, 4, 4,
                             GL_ALPHA, GL_UNSIGNED_BYTE );

    dyeImage->setImage( 4, 4, 4,
                        GL_ALPHA,
                        GL_ALPHA,
                        GL_UNSIGNED_BYTE,
                        dyeTex,
                        osg::Image::USE_NEW_DELETE, 1 );
    dyeImage->setDataVariance( osg::Object::DYNAMIC );
    if( !_dye.valid() )
    {
        _dye = new osg::Texture3D();
        _dye->setFilter( osg::Texture3D::MIN_FILTER,
                         osg::Texture3D::LINEAR );
        _dye->setFilter( osg::Texture3D::MAG_FILTER,
                         osg::Texture3D::LINEAR );
        _dye->setWrap( osg::Texture3D::WRAP_S,
                       osg::Texture3D::CLAMP );
        _dye->setWrap( osg::Texture3D::WRAP_T,
                       osg::Texture3D::CLAMP );
        _dye->setWrap( osg::Texture3D::WRAP_R,
                       osg::Texture3D::CLAMP );

        _dye->setTextureSize( 4, 4, 4 );
        _dye->setInternalFormat( GL_ALPHA );
        _dye->setDataVariance( osg::Object::DYNAMIC );
    }
    _dye->setImage( dyeImage.get() );

}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::_initNoiseTexture()
{
    if( _noiseCbk.valid() )
        return;
    GLuint hI[256];
    GLuint ga[256];
    GLuint gI[256];
    GLfloat taoAlpha = .1;
    GLfloat taoI  = .1;
    GLfloat taoH = .9;
    //build ga
    for( unsigned int i = 0; i < 256; i++ )
    {
        if( i < taoAlpha*255 )
        {
            ga[i] = 0;
        }
        else
        {
            ga[i] = 255.0 * ( i -/*255  */taoAlpha * 255 ) / ( 255.0 - taoAlpha * 255 );
        }
    }

    for( unsigned int i = 0; i < 256; i++ )
    {
        if( i < taoI*255 )
        {
            gI[i] = 0;
        }
        else
        {
            gI[i] = 255;
        }
    }

    //build hI transfer
    for( unsigned int i = 0; i < 256; i++ )
    {
        if( i < taoH*255 )
        {
            hI[i] = 0;
        }
        else
        {
            hI[i] = i;
        }
    }

    unsigned int phase[32][32][32];
    for( unsigned int i = 0; i < 32; i++ )
        for( unsigned int j = 0; j < 32; j++ )
            for( unsigned int k = 0; k < 32; k++ )
                phase[i][j][k] = rand() % 256;

    unsigned char noiseTex[32*32*32*4] ;
    //unsigned int nData = 32*32*32;
    unsigned int t = 0;
    unsigned int index = 0;
    //for (int i = 0; i < nData; i++) {
    for( int i = 0; i < 32; i++ )
    {
        t = i * 256 / 32;
        for( unsigned int j = 0; j < 32; j++ )
        {
            for( unsigned int k = 0; k < 32; k++ )
            {
                /*noiseTex[index*4    ] = (unsigned char)((hI[(phase[i][j][k]+t) % 255])*(ga[(phase[k][j][i]+t) % 255]));
                      
                noiseTex[index*4 + 1] = (unsigned char)phase[i][j][k];
                noiseTex[index*4  +2] = (unsigned char)(ga[(phase[i][j][k] + t) % 255])*
                                                     (hI[(phase[k][j][i] + t) % 255]);
                noiseTex[index*4  +3] = (unsigned char)phase[k][j][i];*/
                noiseTex[index*4    ] = ( unsigned char )255;//((hI[(phase[i][j][k]) % 255]));

                noiseTex[index*4 + 1] = ( unsigned char )( phase[i][j][k] );
                noiseTex[index*4  +2] = ( unsigned char )255;//(hI[(phase[k][j][i]) % 255]);
                noiseTex[index*4  +3] = ( unsigned char )( ga[( phase[k][j][i] + t ) % 255] );
                index++;
            }
        }
    }

    _noiseTexture = new osg::Texture3D();
    _noiseTexture->setDataVariance( osg::Object::DYNAMIC );
    _noiseTexture->setFilter( osg::Texture3D::MIN_FILTER,
                              osg::Texture3D::NEAREST );
    _noiseTexture->setFilter( osg::Texture3D::MAG_FILTER,
                              osg::Texture3D::NEAREST );
    _noiseTexture->setWrap( osg::Texture3D::WRAP_S,
                            osg::Texture3D::REPEAT );
    _noiseTexture->setWrap( osg::Texture3D::WRAP_T,
                            osg::Texture3D::REPEAT );
    _noiseTexture->setWrap( osg::Texture3D::WRAP_R,
                            osg::Texture3D::REPEAT );
    _noiseTexture->setTextureSize( 32, 32, 32 );
    _noiseTexture->setInternalFormat( GL_RGBA );

    osg::ref_ptr<osg::Image> noiseyImage = new osg::Image();
    noiseyImage->setImage( 32, 32, 32,
                           GL_RGBA,
                           GL_RGBA,
                           GL_UNSIGNED_BYTE,
                           noiseTex,
                           osg::Image::USE_NEW_DELETE, 1 );
    noiseyImage->setDataVariance( osg::Object::DYNAMIC );

    _noiseTexture->setImage( noiseyImage.get() );
    _noiseTexture->setTextureSize( 32, 32, 32 );

    _noiseCbk = new cfdUpdateableOSGNoiseTexture3d();
    _noiseTexture->setSubloadCallback( _noiseCbk.get() );
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::_initWeightFunctions()
{
    /*if(_weightW.valid()&&_weightV.valid())return;
    //this does nothing for now. . .can be functions that are
    //weighted based on velocity
    osg::ref_ptr<osg::Image> tempW = new osg::Image();
    osg::ref_ptr<osg::Image> tempV = new osg::Image();

    tempW->allocateImage(2,2,2,GL_RGBA,GL_UNSIGNED_BYTE);
    unsigned char* data = new unsigned char[32];
    unsigned char* data2= new unsigned char[32];
    for(int p = 0; p < 8; p++){
       
          data[p*4   ] = (unsigned char)0;

          data[p*4 + 1] = (unsigned char)0;
          data[p*4 + 2] = (unsigned char)0;
      
          data[p*4 + 3] = (unsigned char)0;   
    }
    for(int p = 0; p < 8; p++){
       
          data2[p*4   ] = (unsigned char)0;

          data2[p*4 + 1] = (unsigned char)0;
          data2[p*4 + 2] = (unsigned char)0;
      
          data2[p*4 + 3] = (unsigned char)0;   
    }
    tempW->setImage(2,2,2,GL_RGBA, GL_RGBA, GL_UNSIGNED_BYTE,
                            data,
                            osg::Image::USE_NEW_DELETE,1);
    tempW->setDataVariance(osg::Object::DYNAMIC);

    tempV->allocateImage(2,2,2,GL_RGBA,GL_UNSIGNED_BYTE);
    tempV->setImage(2,2,2,GL_RGBA, GL_RGBA, GL_UNSIGNED_BYTE,
                            data2,
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
    _weightV->setImage(tempV.get());*/
}
////////////////////////////////////////////////////////////////////////////////
void cfdOSGAdvectionShaderManager::_initLookUpFunction()
{
    if( _lookUpFunction.valid() )
        return;
    GLfloat theta = 0;
    GLfloat value = -1;
    GLfloat dI = 1.0 / 255.0;
    GLint hI[256];
    GLint gI[256];
    for( int i = 0; i < 256; i++ )
    {
        if( i < .9*255 )
        {
            gI[i] = 0;
        }
        else
        {
            gI[i] = 255;
        }
    }

    //build hI transfer
    for( unsigned int i = 0; i < 256; i++ )
    {
        if( i > .1*255 )
        {
            hI[i] = 0;
        }
        else
        {
            hI[i] = 255 - i;
        }
    }

    unsigned char* lutex = new unsigned char[256];
    GLfloat deltaH = 1.0 / 255.0;

    for( int i = 0; i < 256; i++ )
    {
        if( i < 256 )
        {
            theta = i * ( PI / 255.0 );
            value = 255 * sin( theta );
        }
        else
        {
            value = 0;//255-i;
        }
        lutex[i] = ( GLubyte )( value );//hI[i];
    }

    _lookUpFunction = new osg::Texture1D();
    osg::ref_ptr<osg::Image> data = new osg::Image();
    data->allocateImage( 256, 1, 1, GL_ALPHA, GL_UNSIGNED_BYTE );
    data->setImage( 256, 1, 1, GL_ALPHA, GL_ALPHA, GL_UNSIGNED_BYTE, lutex,
                    osg::Image::USE_NEW_DELETE );
    data->setDataVariance( osg::Object::DYNAMIC );

    osg::ref_ptr<osg::Texture1D>trans = new osg::Texture1D;
    trans->setDataVariance( osg::Object::DYNAMIC );
    _lookUpFunction->setTextureWidth( 256 );
    _lookUpFunction->setFilter( osg::Texture1D::MIN_FILTER,
                                osg::Texture1D::LINEAR );
    _lookUpFunction->setFilter( osg::Texture1D::MAG_FILTER,
                                osg::Texture1D::LINEAR );
    _lookUpFunction->setWrap( osg::Texture1D::WRAP_S,
                              osg::Texture3D::CLAMP );
    _lookUpFunction->setInternalFormat( GL_ALPHA );
    _lookUpFunction->setDataVariance( osg::Object::DYNAMIC );
    _lookUpFunction->setImage( data.get() );
}
////////////////////////////////////////////////////////////////////////////////
cfdOSGAdvectionShaderManager& cfdOSGAdvectionShaderManager::operator=( const
        cfdOSGAdvectionShaderManager& sm )
{
    if( this != &sm )
    {
        _velocity = sm._velocity;
        _propertyToAdvect = sm._velocity;
        _lookUpFunction = sm._lookUpFunction;
        _reinit = sm._reinit;
        _dye = sm._dye;
        /*if(_noiseCbk){
           delete _noiseCbk;
          _noiseCbk = 0;
        }*/
        _noiseCbk = sm._noiseCbk;

        _fieldSize[0] = sm._fieldSize[0];
        _fieldSize[1] = sm._fieldSize[1];
        _fieldSize[2] = sm._fieldSize[2];

        /*if(_noiseScaleCallback){
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
        }*/
        _noiseScaleCallback = sm._noiseScaleCallback;
        _deltaCallback = sm._deltaCallback;
        _timeCallback = sm._timeCallback;
        _periodCallback = sm._periodCallback;
        _dyeScaleCallback = sm._dyeScaleCallback;
        _dyeTransCallback = sm._dyeTransCallback;

        _weightV[0] = sm._weightV[0];
        _weightV[1] = sm._weightV[1];
        _weightV[2] = sm._weightV[2];
        _weightV[3] = sm._weightV[3];

        _weightW[0] = sm._weightW[0];
        _weightW[1] = sm._weightW[1];
        _weightW[2] = sm._weightW[2];
        _weightW[3] = sm._weightW[3];
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
