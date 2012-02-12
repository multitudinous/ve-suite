/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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



// --- My Includes --- //
#include <ves/xplorer/network/VE_i.h>
#include "Multiscale2GraphicalPlugin.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/shader/Shader.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/network/GraphicalPluginManager.h>

#include <gadget/Event/KeyboardMouse/KeyEvent.h>
#include <gadget/Event/KeyboardMouse/MouseEvent.h>

#include <gadget/Type/KeyboardMouseInterface.h>

#include <limits>
#include <cassert>
#include <iostream>
#include <fstream>
#include <cstdio>
#include <string>
//#include <stdint.h>
#include <math.h>

#include <sys/types.h>
#include <sys/stat.h>


#include <ves/xplorer/util/readWriteVtkThings.h>

// --- OSG Includes --- //
#include <osg/MatrixTransform>
#include <osg/AnimationPath>
#include <osg/ShapeDrawable>
#include <osg/Sequence>
#include <osg/Multisample>

#include <osg/DisplaySettings>

#include <osgText/Text>

#include <osgDB/ReadFile>
#include <osgDB/FileUtils>

#include <osgSim/ColorRange>
#include <osg/Vec3d>

#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Program>
#include <osg/Shader>
#include <osg/Point>
#include <osg/ClampColor>
#include <osg/Switch>
#include <osg/VertexProgram>

#include <boost/lexical_cast.hpp>

#include <osg/PointSprite>

#include "atoms.h"


#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkDataSet.h>
#include <vtkCutter.h>
#include <vtkPolyDataMapper.h>
#include <vtkPolyData.h>
#include <vtkCellDataToPointData.h>
#include <vtkActor.h>
#include <vtkProperty.h>
#include <vtkCellArray.h>
#include <vtkDoubleArray.h>
#include <vtkPointData.h>

#include <ves/xplorer/scenegraph/Geode.h>


#include <string>
#include <stdio.h>


#include <stdlib.h>


using namespace opcgp;

#define __DO_LATTICE__




//converts HSV TO RGB used for color ramp functionality
//integer conversion
//taken from http://www.nunosantos.net/archives/114 no license given
void HSVtoRGB( int  *r, int *g,int *b, int h, int s, int v )
{
	int f;
	long p, q, t;
	
	if( s == 0 )
	{
		*r = *g = *b = v;
		return;
	}
	
	f = ((h%60)*255)/60;
	h /= 60;

	p = (v * (256 - s))/256;
	q = (v * ( 256 - (s * f)/256 ))/256;
	t = (v * ( 256 - (s * ( 256 - f ))/256))/256;
	
	switch( h ) {
		case 0:
			*r = v;
			*g = t;
			*b = p;
			break;
		case 1:
			*r = q;
			*g = v;
			*b = p;
			break;
		case 2:
			*r = p;
			*g = v;
			*b = t;
			break;
		case 3:
			*r = p;
			*g = q;
			*b = v;
			break;
		case 4:
			*r = t;
			*g = p;
			*b = v;
			break;
		default:
			*r = v;
			*g = p;
			*b = q;
			break;
	}
}

////////////////////////////////////////////////////////////////////////////////
Multiscale2GraphicalPlugin::Multiscale2GraphicalPlugin()
    :
    ves::xplorer::plugin::PluginBase(),
    m_keyboard( 0 )
{
    //DYNSIM
    mObjectName = "Multiscale2";
    mEventHandlerMap[ "OPCData" ] = this;
    mEventHandlerMap[ "TANK_CAD" ] = this;
	mEventHandlerMap[ "MULTISCALE_COMMAND" ] = this;
    //mEventHandlerMap[ "VALVE_CAD" ] = this;
    m_tankDCS = 0;
	animating = true;
	
	
}
////////////////////////////////////////////////////////////////////////////////
Multiscale2GraphicalPlugin::~Multiscale2GraphicalPlugin()
{
}
////////////////////////////////////////////////////////////////////////////////
void Multiscale2GraphicalPlugin::InitializeNode( osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );
	
	//start off displaying the lattice and not the dislocations because the lattice looks quite a bit better
	latticeEnabled = true;
	dislocationsEnabled = false;
        
	
	leswitch = new osg::Switch;
	latticeSwitch = new osg::Switch;
	osg::Geometry* geometry;// = new osg::Geometry;
	osg::Geode* geode;// = new osg::Geode;
	
	//CALL AARON IF YOU NEED TO USE DEM DATA AND HE WILL HELP YOU CHANGE THE NECESSARY CONSTANTS
	for(int ii = 1; ii < 71; ii++) {
		
		geometry = new osg::Geometry;
		geode = new osg::Geode;
		vertices = new osg::Vec3Array;
		//osg::Vec3Array* normals = new osg::Vec3Array;
		osg::Vec4ubArray* colours = new osg::Vec4ubArray;
		
		osg::Vec3 pos;
		//osg::Vec3 normal(0.0,0.0,1.0);
		int r=255,g=255,b=255,a=255;
		
		
#ifdef __DO_LATTICE__
		atoms_t* atoms = new atoms_t(10000000);
		std::string fnamebase = "/Volumes/MacOS_Data/material_demo/traj/filtered/dat.";
		char suffix[4];
		sprintf(suffix, "%d",ii);
		std::string fn = fnamebase + suffix;
		atoms->Read(fn);
		
		for(uint i = 0; i < atoms->Size(); i++) {
			if(atoms->GetCentrosymmetry(i) > 1) {
				pos.x() = atoms->GetCenterX(i);
				pos.y() = atoms->GetCenterY(i);
				pos.z() = atoms->GetCenterZ(i);
				vertices->push_back(pos);
				//normals->push_back(normal);
				r = int( atoms->GetCentrosymmetry(i)*20.0 );
				if(i%100000==0)
					std::cout << atoms->GetCentrosymmetry(i) << " ";
				if(r>255)
					r=255;
				g = 255-r;
				//r = i%255;
				colours->push_back(osg::Vec4ub(r,g,b,a));
				
			}
		}
#else
		
		std::string fnamebase = "/Volumes/MacOS_Data/material_demo/dem/release/dat.";
		char suffix[4];
		sprintf(suffix, "%d",ii);
		std::string fn = fnamebase + suffix;

		struct stat filestatus;
		
		stat( fn.c_str(), &filestatus );
		std::cout <<fn << std::endl;
    
		uint numAtoms = filestatus.st_size/(4*sizeof(float));
		float* buffer, *buffer2;
		std::cout << numAtoms << std::endl;
		buffer = new float[numAtoms*4*5];
		buffer2 = new float[numAtoms*4*5];
		FILE* fp, *fp2;
		fp2 = fopen("/Volumes/MacOS_Data/material_demo/dem/release/dat.1","rb");
		fp = fopen(fn.c_str(), "rb");
		fread(buffer, numAtoms, sizeof(float)*4, fp);
		fread(buffer2, numAtoms, sizeof(float)*4, fp2);

		for(uint i = 0; i < numAtoms; i++) {
		
				pos.x() = buffer[i*4];
				pos.y() = buffer[i*4+1];
				pos.z() = buffer[i*4+2];
				vertices->push_back(pos);
							
			int h, s, v;
			float height = buffer2[i*4+1];
			height = height*255;
			height+=128;
			h = (int)height;
		
			
			if (h > 255) h = 255;
			if (h < 0) h = 0;
			s = 128;
			v = 256;
		
			HSVtoRGB( &r, &g,&b, h, s, v );
			colours->push_back(osg::Vec4ub(r,g,b,a));
		}
		
		delete buffer;
		fclose(fp);
		fclose(fp2);
		delete buffer2;
		
		
		
#endif
		

		geometry->setUseDisplayList(false);    
		geometry->setUseVertexBufferObjects(false);    
		geometry->setVertexArray(vertices);
		//geometry->setNormalArray(normals);
		//geometry->setNormalBinding(osg::Geometry::BIND_PER_VERTEX);
		geometry->setColorArray(colours);
		geometry->setColorBinding(osg::Geometry::BIND_PER_VERTEX);
		geometry->addPrimitiveSet(new osg::DrawArrays(GL_POINTS,0,vertices->size()));
		
		
		
		geode->addDrawable(geometry);
		latticeSwitch->addChild(geode,true);
		
		#ifdef __DO_LATTICE__
		
		delete atoms;
#endif
		
		
	}
	m_tankDCS = new ves::xplorer::scenegraph::DCS();
	
	
	//this controls which frame is shown
	
	mDCS->addChild( m_tankDCS.get() );
	osg::StateSet* stateset = latticeSwitch->getOrCreateStateSet();
	stateset->setMode(GL_MULTISAMPLE_ARB, osg::StateAttribute::ON);
	
	osg::DisplaySettings * ds = osg::DisplaySettings::instance();
ds->setNumMultiSamples(16);

#if 1
	stateset->setMode(GL_BLEND, osg::StateAttribute::OFF);
	
	osg::PointSprite *sprite = new osg::PointSprite();
	stateset->setTextureAttributeAndModes(0, sprite, osg::StateAttribute::ON);
	stateset->setMode(GL_VERTEX_PROGRAM_POINT_SIZE, osg::StateAttribute::ON);
	
	
	
	osg::ClampColor* clamp = new osg::ClampColor(); 
	clamp->setClampVertexColor(GL_FALSE); 
	clamp->setClampFragmentColor(GL_FALSE); 
	
	
	
	
	
	
	
		
		
		        osg::Program* program = new osg::Program;
		        
		
		        
	
	std::string fileName = "sphere.vs";
    osg::Shader* shader = osg::Shader::readShaderFile( osg::Shader::VERTEX,
													  osgDB::findDataFile( fileName ) );
    if( shader == NULL )
        osg::notify( osg::WARN ) << "Can't load " << fileName << std::endl;
    shader->setName( fileName );
    program->addShader( shader );
	
	
	fileName = "Basic.fsh";
    shader = osg::Shader::readShaderFile( osg::Shader::FRAGMENT,
										 osgDB::findDataFile( fileName ) );
    if( shader == NULL )
        osg::notify( osg::WARN ) << "Can't load " << fileName << std::endl;
    shader->setName( fileName );
    program->addShader( shader );
	stateset->setAttribute(program);
	
#endif
	
	//lines data loading
	for(int ii = 1 ; ii < 98;ii++) {
		
		std::string fnamebase = "/Volumes/MacOS_Data/material_demo/srd1d/converted-binary_";
		char suffix[4];
		sprintf(suffix, "%d",ii);
		std::string fn = fnamebase + suffix;
		std::string extension = ".vtp";
		fn = fn + extension;
		
		vtkPolyData* pd = dynamic_cast< vtkPolyData* >( ves::xplorer::util::readVtkThing( fn, 0 ) );
		
		vtkPolyDataMapper* mapper = vtkPolyDataMapper::New();
		mapper->SetInput( pd );
				
		vtkActor* contActor = vtkActor::New();
		contActor->SetMapper(mapper);
		
		
		
		 ves::xplorer::scenegraph::Geode* tempGeode = 
        new ves::xplorer::scenegraph::Geode();
		tempGeode->TranslateToGeode( contActor );
		
		leswitch->addChild( tempGeode, true );
	}
	
	m_tankDCS->addChild(leswitch);
	m_tankDCS->addChild(latticeSwitch);
	
	leswitch->setSingleChildOn(5);
	latticeSwitch->setAllChildrenOff();
	
	framecount = 0;
	latticeEnabled = true;
	dislocationsEnabled = false;
	latticeSwitch->setAllChildrenOff();
	latticeSwitch->setNodeMask(0xffffffff);
	leswitch->setNodeMask(0x0);
	animating = true;
}
////////////////////////////////////////////////////////////////////////////////
void Multiscale2GraphicalPlugin::PreFrameUpdate()
{
    	
	//if drawing the lattice turn the correct lattice frame on
	if(latticeEnabled) {
		latticeSwitch->setSingleChildOn((framecount/6)%71);
	}
	
	//if drawing the dislocations turn on the correct dislocation frame
	if(dislocationsEnabled) {
		leswitch->setSingleChildOn((framecount/6)%98);
	}
	
	//if animating increment the frame count
	//it may be desirable to change this to time based animation
	if(animating) {
		framecount++;
	}
}
////////////////////////////////////////////////////////////////////////////////
void Multiscale2GraphicalPlugin::SetCurrentCommand(
    ves::open::xml::CommandPtr command )
{
	std::cout << "command recieved" << std::endl;
    if( !command )
    {
        return;
    }

    //currently there is no functional flowsheet with a tank
    //check for the tank dvp
    if( command->GetDataValuePair("MY_VALVE") )  //change to "MY_TANK"
    {
        //the value for the tank level
        std::string percent;
        //change to "MY_TANK"
        command->GetDataValuePair("MY_VALVE")->GetData( percent );
        m_tankLevel = boost::lexical_cast<double>( percent );
    }
	if(command->GetDataValuePair("ENABLE_LATTICE")) {
		framecount = 0;
		latticeEnabled = true;
		dislocationsEnabled = false;
		leswitch->setAllChildrenOff();
		leswitch->setNodeMask(0x0);
		latticeSwitch->setNodeMask(0xffffffff);
	}
	
	if(command->GetDataValuePair("ENABLE_DISLOCATIONS")) {
		framecount = 0;
		latticeEnabled = false;
		dislocationsEnabled = true;
		latticeSwitch->setAllChildrenOff();
		leswitch->setNodeMask(0xffffffff);
		latticeSwitch->setNodeMask(0x0);
	}
	
	if(command->GetDataValuePair("DISABLE_ALL")) {
		framecount = 0;
		latticeEnabled = false;
		dislocationsEnabled = false;
		latticeSwitch->setAllChildrenOff();
		leswitch->setNodeMask(0x0);
		latticeSwitch->setNodeMask(0x0);
	}
	
	if(command->GetDataValuePair("SET_FRAME")) {
		framecount = 0;
		std::string frame;
		command->GetDataValuePair("SET_FRAME")->GetData( frame );
		std::cout << frame << std::endl;
		framecount = 6*((int) atoi(frame.c_str()) );
		
		
	}
	
	
	if(command->GetDataValuePair("TOGGLE_ANIMATION")) {
		animating = !animating;
	}
    }
////////////////////////////////////////////////////////////////////////////////
void Multiscale2GraphicalPlugin::SetCurrentCommands(
    std::vector< ves::open::xml::CommandPtr > const& commands )
{
    if( commands.empty() )
    {
        return;
    }
    ves::open::xml::CommandPtr tempPtr = commands.at( commands.size() - 1 );
    SetCurrentCommand( tempPtr );
}
////////////////////////////////////////////////////////////////////////////////
void Multiscale2GraphicalPlugin::FindPartNodeAndHighlightNode()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
