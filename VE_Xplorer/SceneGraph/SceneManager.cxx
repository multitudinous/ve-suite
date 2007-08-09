/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef WIN32
#include <sys/types.h>
//biv--check here if build/run problems occur
#else
//#include <windows.h>
#endif

// --- VE-Suite Includes --- //
#include "VE_Xplorer/SceneGraph/SceneManager.h"

#include "VE_Xplorer/SceneGraph/CADEntity.h"

#include "VE_Xplorer/SceneGraph/Logo/BlueArrow.h"
#include "VE_Xplorer/SceneGraph/Logo/GreyArrow.h"
#include "VE_Xplorer/SceneGraph/Logo/OrangeArrow.h"
#include "VE_Xplorer/SceneGraph/Logo/VE.h"
#include "VE_Xplorer/SceneGraph/Logo/Suite.h"

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/Group>
#include <osg/Node>
#include <osgDB/Registry>
#include <osgDB/ReaderWriter>
#include <osgDB/WriteFile>
#include <osg/PositionAttitudeTransform>
#include <osg/MatrixTransform>
#include <osg/Switch>
#endif

// --- C/C++ Libraries --- //
#include <iostream>
#include <string>
#include <istream>
#include <sstream>

using namespace VE_SceneGraph;

vprSingletonImp( SceneManager );

SceneManager::SceneManager( void )
{
   this->_param.erase();

   m_blueArrow = 0;
   m_greyArrow = 0;
   m_orangeArrow = 0;
   m_veText = 0;
   m_suiteText = 0;
}
//////////////////////////////////////////////////////////
void SceneManager::Initialize( std::string param )
{
   _param = param;
}
///////////////////////////////////////////////////
void SceneManager::CleanUp( void )
{
   //Do nothing right now
   if( m_blueArrow )
   {
      delete m_blueArrow;
      m_blueArrow = 0;
   }
	
   if( m_greyArrow )
   {
      delete m_greyArrow;
      m_greyArrow = 0;
   }

   if( m_orangeArrow )
   {
      delete m_orangeArrow;
      m_orangeArrow = 0;
   }

   if( m_veText )
   {
      delete m_veText;
      m_veText = 0;
   }

   if( m_suiteText )
   {
      delete m_suiteText;
      m_suiteText = 0;
   }
}

void SceneManager::InitScene( void )
{
   std::cout << "|  1. Initializing................................ Performer scenes |" << std::endl;

   this->rootNode = new VE_SceneGraph::Group();
   this->rootNode->SetName( "Root Node" );
   rootNode->setThreadSafeRefUnref( true );

   this->worldDCS = new VE_SceneGraph::DCS();
   this->worldDCS->SetName( "World DCS" );

   this->networkDCS  = new VE_SceneGraph::DCS();
   this->networkDCS->SetName( "Network DCS" );

   m_clrNode = new osg::ClearNode();
   m_clrNode->setRequiresClear( true );
   m_clrNode->setClearColor( osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
   m_clrNode->setName( "Clear Node - Control ClearColor" );
   
   //Create the switch for our logo
   _createLogo();

   _logoSwitch->AddChild( worldDCS.get() );
   _logoSwitch->AddChild( _logoNode.get() );
   _logoSwitch->AddChild( networkDCS.get() );   

#ifdef VE_PATENTED   
    m_oqc = new osgOQ::OcclusionQueryContext();
    ///number of pixels
    m_oqc->setVisibilityThreshold( 500 );
    ///Number of verts
    m_oqc->setOccluderThreshold( 1000 );
    ///Specifies how many frames to wait before issuing another query    
    m_oqc->setQueryFrameCount( 5 );
    ///Specify whether to use hierarchical ("NonFlat") placement for
    m_oqc->setNonFlatPlacement( true );
    ///Place bounding volumes in for osgOQ nodes
    m_oqc->setDebugDisplay( true );
    // Sets the debug verbosity. Currently supported 'level' values:
    //    0 -- Verbosity is controlled by osg::notify.
    //    1 -- For each OQN in each frame, displays whether that node
    //         thinks its actual geometry is visible or not and why.
    // Call through OcclusionQueryRoot to set value only for a
    //   specific number of frames.
    //void setDebugVerbosity( 0 );
    m_oqc->setStatistics( true );
#endif    

    rootNode->addChild( m_clrNode.get() );
    m_clrNode->addChild( _logoSwitch.get() );
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::Group* SceneManager::GetRootNode( void )
{
   return this->rootNode.get();
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* SceneManager::GetWorldDCS( void )
{
   return this->worldDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* SceneManager::GetNetworkDCS( void )
{
   return networkDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::ViewLogo( bool trueFalse )
{
   if( trueFalse )
   {
      SetActiveSwitchNode( 1 );
   }

   else
   {
      SetActiveSwitchNode( 0 );
   }
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::_createLogo()
{
#ifdef _OSG
   if( !_logoSwitch )
   {
      _logoSwitch = new VE_SceneGraph::Switch();   
   }
   
   if( !_logoNode.valid() )
   {
      double translation[3] = { -1.5, 5, 3.2 };
      double scale[3] = { 0.0065, 0.0065, 0.0065 };

      _logoNode = new VE_SceneGraph::DCS();
      _logoNode->SetTranslationArray( translation );
      _logoNode->SetScaleArray( scale );

      m_blueArrow = new VE_SceneGraph::CADEntity( BlueArrow(), _logoNode.get(), true, false );
      m_greyArrow = new VE_SceneGraph::CADEntity( GreyArrow(), _logoNode.get(), true, false );
      m_orangeArrow = new VE_SceneGraph::CADEntity( OrangeArrow(), _logoNode.get(), true, false );
      m_veText = new VE_SceneGraph::CADEntity( VE(), _logoNode.get(), true, false );
      m_suiteText = new VE_SceneGraph::CADEntity( Suite(), _logoNode.get(), true, false );

        char phong_vertex[]=
            "varying vec4 color; \n"
            "varying vec3 eyePos; \n"
            "varying vec3 lightPos; \n"
            "varying vec3 normal; \n"

            "void main() \n"
            "{ \n"
                 "gl_Position=ftransform(); \n"

                 "color=gl_Color; \n"
                 "eyePos=vec3(gl_ModelViewMatrix*gl_Vertex); \n"
                 "lightPos=gl_LightSource[0].position.xyz; \n"
                 "normal=vec3(gl_NormalMatrix*gl_Normal); \n"
            "} \n";

        char phong_fragment[]=
        "varying vec4 color; \n"
        "varying vec3 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"   
             "vec3 N=normalize(normal); \n"
             "vec3 L=normalize(lightPos); \n"
             "float NDotL=max(dot(N,L),0.0); \n"

             "vec3 V=normalize(eyePos); \n"
             "vec3 R=reflect(V,N); \n"
             "float RDotL=max(dot(R,L),0.0); \n"

             "vec3 TotalAmbient=gl_LightSource[0].ambient.rgb*color.rgb; \n"
             "vec3 TotalDiffuse=gl_LightSource[0].diffuse.rgb*color.rgb*NDotL; \n"
             "vec3 TotalSpecular=gl_LightSource[0].specular.rgb*color.rgb*pow(RDotL,20.0); \n"

             "vec3 color=TotalAmbient+TotalDiffuse+TotalSpecular; \n"

             "gl_FragColor=vec4(color,1.0); \n"
        "} \n";

        osg::ref_ptr< osg::StateSet > stateset = _logoNode->getOrCreateStateSet();
        osg::ref_ptr< osg::Program > program = new osg::Program;

        osg::ref_ptr< osg::Shader > vertex_shader = new osg::Shader( osg::Shader::VERTEX, phong_vertex );
        program->addShader( vertex_shader.get() );

        osg::ref_ptr< osg::Shader > fragment_shader = new osg::Shader( osg::Shader::FRAGMENT, phong_fragment );
        program->addShader( fragment_shader.get() );

        stateset->setAttribute( program.get() );
   }
#endif
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetActiveSwitchNode( int activeNode )
{
   _logoSwitch->SetVal( activeNode );
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::PreFrameUpdate( void )
{
   networkDCS->SetTranslationArray( worldDCS->GetVETranslationArray() );
   networkDCS->SetRotationArray( worldDCS->GetRotationArray() );
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* SceneManager::GetActiveSwitchNode( void )
{
   osg::Switch::ValueList boolList = _logoSwitch->getValueList();
   
   for( size_t i = 0; i < boolList.size(); i++ )
   {
      if( boolList.at( i ) )
      {
         return dynamic_cast< VE_SceneGraph::DCS* >( _logoSwitch->getChild( i ) );
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetBackgroundColor(std::vector<double> color)
{
    m_clrNode->setClearColor( osg::Vec4( color.at(0), color.at(1), color.at(2), 1.0f ) );
}
////////////////////////////////////////////////////////////////////////////////
#ifdef VE_PATENTED
osgOQ::OcclusionQueryContext* SceneManager::GetOcclusionQueryContext()
{
    return m_oqc.get();
}
#endif
////////////////////////////////////////////////////////////////////////////////
void SceneManager::ResetOcclusionQueryContext()
{
#ifdef VE_PATENTED
    m_oqc = new osgOQ::OcclusionQueryContext();
    ///number of pixels
    m_oqc->setVisibilityThreshold( 500 );
    ///Number of verts
    m_oqc->setOccluderThreshold( 1000 );
    ///Specifies how many frames to wait before issuing another query    
    m_oqc->setQueryFrameCount( 5 );
    ///Specify whether to use hierarchical ("NonFlat") placement for
    m_oqc->setNonFlatPlacement( true );
    ///Place bounding volumes in for osgOQ nodes
    m_oqc->setDebugDisplay( true );
    // Sets the debug verbosity. Currently supported 'level' values:
    //    0 -- Verbosity is controlled by osg::notify.
    //    1 -- For each OQN in each frame, displays whether that node
    //         thinks its actual geometry is visible or not and why.
    // Call through OcclusionQueryRoot to set value only for a
    //   specific number of frames.
    //void setDebugVerbosity( 0 );
    m_oqc->setStatistics( true );
#endif
}
