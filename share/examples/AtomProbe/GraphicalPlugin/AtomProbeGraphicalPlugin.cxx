/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
 * Date modified: $Date: 2011-10-07 16:20:34 -0500 (Fri, 07 Oct 2011) $
 * Version:       $Rev: 16404 $
 * Author:        $Author: tjordan $
 * Id:            $Id: AtomProbeGraphicalPlugin.cxx 16404 2011-10-07 21:20:34Z tjordan $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/



// --- My Includes --- //
#include <ves/xplorer/network/VE_i.h>
#include "AtomProbeGraphicalPlugin.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/shader/Shader.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/device/KeyboardMouse.h>
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

#include <limits>

#include "AtomTypes.h"

using namespace opcgp;

//#define __DO_LATTICE__




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
AtomProbeGraphicalPlugin::AtomProbeGraphicalPlugin()
    :
    ves::xplorer::plugin::PluginBase(),
    m_keyboard( 0 )
{
    //DYNSIM
    mObjectName = "AtomProbe";
    mEventHandlerMap[ "OPCData" ] = this;
    mEventHandlerMap[ "TANK_CAD" ] = this;
	mEventHandlerMap[ "MULTISCALE_COMMAND" ] = this;
    //mEventHandlerMap[ "VALVE_CAD" ] = this;
    m_tankDCS = 0;
	animating = true;
	std::vector<double> bgcolor;
	bgcolor.push_back(1.0);
	bgcolor.push_back(1.0);
	bgcolor.push_back(1.0);
	
	ves::xplorer::scenegraph::SceneManager::instance()->SetBackgroundColor(bgcolor);
	ves::xplorer::eventmanager::EventManager* evm =
	ves::xplorer::eventmanager::EventManager::instance();
	using ves::xplorer::eventmanager::SignalWrapper;
    
    {
        std::string signalName = "AtomProbeGraphicalPlugin" +
		boost::lexical_cast<std::string>( this ) + ".HistogramDataSignal";
        evm->RegisterSignal(
							new SignalWrapper< ColorDoubleVectorSignal_type >( &m_HistogramDataSignal ),
							signalName, ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
    }
	
	{
        std::string signalName = "AtomProbeGraphicalPlugin" +
		boost::lexical_cast<std::string>( this ) + ".MaskHistogramDataSignal";
        evm->RegisterSignal(
							new SignalWrapper< DoubleVectorSignal_type >( &m_MaskHistogramDataSignal ),
							signalName, ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
    }
	{
        std::string signalName = "AtomProbeGraphicalPlugin" +
		boost::lexical_cast<std::string>( this ) + ".NumPulses";
        evm->RegisterSignal(
							new SignalWrapper< ves::util::DoubleSignal_type >( &m_NumPulsesSignal ),
							signalName, ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
    }
	
	
	
}
////////////////////////////////////////////////////////////////////////////////
AtomProbeGraphicalPlugin::~AtomProbeGraphicalPlugin()
{
}
////////////////////////////////////////////////////////////////////////////////
void AtomProbeGraphicalPlugin::InitializeNode( osg::Group* veworldDCS )
{
	
	
	//initialize hit toggles and colors. I really don't know why this worked before this was moved.
	for(int i = 0; i < 4; i++) {
		rrHit.push_back(1);
		ggHit.push_back(1);
		bbHit.push_back(1);
		hitEnabled.push_back(true);
	}
	
	
	
	
	
	
    PluginBase::InitializeNode( veworldDCS );
	
	//start off displaying the lattice and not the dislocations because the lattice looks quite a bit better
	latticeEnabled = true;
	dislocationsEnabled = false;
    m_keyboard = dynamic_cast< ves::xplorer::device::KeyboardMouse* >( mDevice );
        
	
	leswitch = new osg::Switch;
	latticeSwitch = new osg::Switch;
	osg::Geometry* geometry;// = new osg::Geometry;
	osg::Geode* geode;// = new osg::Geode;
	
	
	//initialize internal color array
	colours = new osg::Vec4ubArray;
	coloursOnCard = new osg::Vec4ubArray;
	
	//CALL AARON IF YOU NEED TO USE DEM DATA AND HE WILL HELP YOU CHANGE THE NECESSARY CONSTANTS
	for(int ii = 1; ii < 2; ii++) {
		
		geometry = new osg::Geometry;
		geode = new osg::Geode;
		vertices = new osg::Vec3Array;
		osg::Vec3Array* normals = new osg::Vec3Array;
		osg::Vec4ubArray* colours2 = new osg::Vec4ubArray;
		
		osg::Vec3 pos;
		//osg::Vec3 normal(0.0,0.0,1.0);
		int r=255,g=255,b=255,a=255;
		
		currentBottom = 0;
		currentTop = 1;
		currentVariable = 0;
		
		
		currentMaskTop = 1;
		currentMaskBottom = 0;
		currentMaskVariable = 0;
		
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
				r = atoms->GetCentrosymmetry(i)*20;
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
		Atoms * atoms = new Atoms();
		//FeNi Perpendicular-v02.epos
		//atoms->loadEPOS("/Volumes/Data/FeNi Perpendicular-v02.
		atoms->loadEPOS("/Volumes/Data/R12_05273-v01.epos");
		
		//atoms->loadEPOS("/Volumes/Data/R12_Al-Sc.epos");
		//atoms->loadEPOS("/Volumes/Data/Cu-Si_anneal.epos");
		std::cout << "made it to here" << std::endl;
		_numAtoms = atoms->getNumAtoms();
		rawData = (float*)atoms->getRawData();
		/*std::string fnamebase = "/Volumes/MacOS_Data/material_demo/dem/release/dat.";
		char suffix[4];
		sprintf(suffix, "%d",ii);
		std::string fn = fnamebase + suffix;
		//fn = "/Volumes/Data/R12_Al-Sc.epos";
		fn = "/Volumes/Data/R12_05273-v01.epos";
		//fn = "/Volumes/Data/FeNi Perpendicular-v02.epos";
		struct stat filestatus;
		
		stat( fn.c_str(), &filestatus );
		std::cout <<fn << std::endl;
    
		unsigned int numAtoms = filestatus.st_size/(11*sizeof(float));
		_numAtoms = numAtoms;
		float* buffer, *buffer2;
		
		std::cout << numAtoms << std::endl;
		buffer = new float[numAtoms*11*5];
		buffer2 = new float[numAtoms*11*5];
		FILE* fp, *fp2;
		
		
		//fp2 = fopen("/Volumes/Data/FeNi Perpendicular-v02.epos","rb");
		fp2 = fopen("/Volumes/Data/R12_05273-v01.epos","rb");
		//fp2 = fopen("/Volumes/Data/R12_Al-Sc.epos","rb");
		fp = fopen(fn.c_str(), "rb");
		fread(buffer, numAtoms, sizeof(float)*11, fp);
		fread(buffer2, numAtoms, sizeof(float)*11, fp2);
		rawData = buffer;
		 int* intbuffer = ( int*)buffer;
		 
		 */
		int * intbuffer = (int*)rawData;
		float* buffer = (float*)rawData;
		double currentTime = 0;
		for(uint i = 0; i < _numAtoms; i++) {
			//for(int ii = 0; ii <11; ii++) {
			//	SWAP(buffer[i*11+ii]);
			//}
			//for(int ii = 9; ii < 11; ii++) {
		//		if (i<1000)
		//			std::cout << "ii: " << ii << " val:" << intbuffer[i*11+ii] <<std::endl;;
		//		buffer[i*11+ii] = (float)intbuffer[i*11+ii];
				
		//	}
			currentTime+=buffer[i*11+9];
			timeIndex.push_back(currentTime);
				pos.x() = buffer[i*11];
				pos.y() = buffer[i*11+1];
				pos.z() = buffer[i*11+2];
				vertices->push_back(pos);
							
			int h, s, v;
			float height = buffer[i*4+1];
			height = height*255;
			height+=128;
			h = (int)height;
		
			
			if (h > 255) h = 255;
			if (h < 0) h = 0;
			s = 128;
			v = 256;
		
			HSVtoRGB( &r, &g,&b, h, s, v );
			colours2->push_back(osg::Vec4ub(r,g,b,a));
		}
		m_NumPulsesSignal(currentTime);
		maxTime = currentTime;
		//delete buffer;
		//fclose(fp);
		//fclose(fp2);
		//delete buffer2;
		
		atomProbeGeometry = geometry;
		std::cout << "made it past initial adding of stuff" << std::endl;
		//*/
		/*reinitializePipelineData();
		std::cout << "reinitialized pipeline" << std::endl;
		reMask(currentMaskVariable, currentMaskBottom, currentMaskTop); 
		std::cout << " remasked" << std::endl;
		hitMasking(hitEnabled);
		std::cout << " masked on hits" << std::endl;
		reColorize();*/
		std::cout << " recolorized" << std::endl;
#endif
		

		geometry->setUseDisplayList(false);    
		geometry->setUseVertexBufferObjects(false);    
		geometry->setVertexArray(vertices);
		//geometry->setNormalArray(normals);
		//geometry->setNormalBinding(osg::Geometry::BIND_PER_VERTEX);
		geometry->setColorArray(colours2);
		geometry->setColorBinding(osg::Geometry::BIND_PER_VERTEX);
		geometry->addPrimitiveSet(new osg::DrawArrays(GL_POINTS,0,vertices->size()));
		
		
		
		geode->addDrawable(geometry);
		latticeSwitch->addChild(geode,true);
		
		#ifdef __DO_LATTICE__
		
		delete atoms;
#endif
		
		
	}
	m_tankDCS = new ves::xplorer::scenegraph::DCS();
	reinitializePipelineData();
	reColorize(8,0,1);
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
	for(int ii = 1 ; ii < 2;ii++) {
		
		std::string fnamebase = "/Volumes/Data/material_demo/srd1d/converted-binary_";
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
	
	
	
	
	
	CONNECTSIGNAL_1( "%SelectColorizeDataSignal",
                    void( int const& ),
                    &AtomProbeGraphicalPlugin::setColorizeData,
                    m_connections, normal_Priority );
	
	CONNECTSIGNAL_1( "%SetMaskVariableSignal",
                    void( int const& ),
                    &AtomProbeGraphicalPlugin::setMaskVariable,
                    m_connections, normal_Priority );
	CONNECTSIGNAL_1( "%SetMaskBottomSignal",
                    void( double const& ),
                    &AtomProbeGraphicalPlugin::setMaskBottom,
                    m_connections, normal_Priority );
	CONNECTSIGNAL_1( "%SetMaskTopSignal",
                    void( double const& ),
                    &AtomProbeGraphicalPlugin::setMaskTop,
                    m_connections, normal_Priority );
	
	CONNECTSIGNALS_4("%SetHitColorizeSignal",void (std::vector< int > const&,std::vector< int > const&,std::vector< int > const&, std::vector< bool > const&),
					 &AtomProbeGraphicalPlugin::setHitColorization,
					 m_connections, any_SignalType, normal_Priority);
	
	
	
	CONNECTSIGNAL_1( "%SetLastTimeSignal",
                    void( double const& ),
                    &AtomProbeGraphicalPlugin::setLastTime,
                    m_connections, normal_Priority );
	
	
	
	
	
	
	
}
void AtomProbeGraphicalPlugin::setColorizeData(int data) {
	for(int i = 0; i < 100; i++) {
	std::cout << "recieved signal" << std::endl;
	}
	
	std::vector<double> vec;
	for(int i = 0; i < 100; i++)
		vec.push_back(i);
	
}
////////////////////////////////////////////////////////////////////////////////
void AtomProbeGraphicalPlugin::PreFrameUpdate()
{
    	
	//if drawing the lattice turn the correct lattice frame on
	if(latticeEnabled) {
		latticeSwitch->setSingleChildOn((framecount/6)%1);
	}
	
	//if drawing the dislocations turn on the correct dislocation frame
	if(dislocationsEnabled) {
		leswitch->setSingleChildOn((framecount/6)%5);
	}
	
	//if animating increment the frame count
	//it may be desirable to change this to time based animation
	if(animating) {
		framecount++;
	}
	lastTime = maxTime;
	/*if(lastTime > maxTime)
		lastTime = 0;
	else {
		lastTime += 700000;
	}
	vertices->clear();
	colours->clear();
	
	if(colorizeByHits)
		hitColorization(rrHit,ggHit,bbHit, hitEnabled);
	else 
		reColorize();*/
	

}
////////////////////////////////////////////////////////////////////////////////
void AtomProbeGraphicalPlugin::SetCurrentCommand(
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
	
	if(command->GetDataValuePair("SET_DATA")) {
		framecount = 0;
		std::string data;
		command->GetDataValuePair("SET_DATA")->GetData( data );
		std::cout << data << std::endl;
		int datai = ((int) atoi(data.c_str()) );
		currentVariable = datai;
		reinitializePipelineData();
		reMask(currentMaskVariable, currentMaskBottom, currentMaskTop); 
		hitMasking(hitEnabled);
		reColorize();
		colorizeByHits = false;
		
		
	}
	
	if(command->GetDataValuePair("SET_BOTTOM")) {
		framecount = 0;
		std::string data;
		command->GetDataValuePair("SET_BOTTOM")->GetData( data );
		std::cout << data << std::endl;
		float dataf =  atof(data.c_str());
		currentBottom = dataf;
		reinitializePipelineData();
		reMask(currentMaskVariable, currentMaskBottom, currentMaskTop); 
		hitMasking(hitEnabled);
		reColorize();
		colorizeByHits = false;
		
	}
	
	if(command->GetDataValuePair("SET_TOP")) {
		framecount = 0;
		std::string data;
		command->GetDataValuePair("SET_TOP")->GetData( data );
		std::cout << data << std::endl;
		float dataf =  atof(data.c_str() );
		currentTop = dataf;
		reinitializePipelineData();
		reMask(currentMaskVariable, currentMaskBottom, currentMaskTop); 
		hitMasking(hitEnabled);
		reColorize();
		colorizeByHits = false;
		
	}
	
	
	
	if(command->GetDataValuePair("TOGGLE_ANIMATION")) {
		animating = !animating;
	}
    }
////////////////////////////////////////////////////////////////////////////////
void AtomProbeGraphicalPlugin::SetCurrentCommands(
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
void AtomProbeGraphicalPlugin::FindPartNodeAndHighlightNode()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////



void AtomProbeGraphicalPlugin::reinitializePipelineData() {
	vertices->clear();
	colours->clear();
	rr.clear();
	gg.clear();
	bb.clear();
	currentAtoms.clear();
	for(unsigned int i = 0; i < _numAtoms;i++) {
		currentAtoms.push_back(i);
	}

}

void AtomProbeGraphicalPlugin::reColorize(int variable, float bottomClip, float topClip) {
	
	histogram.clear();
	
	float max = -std::numeric_limits<float>::infinity();
	float min = std::numeric_limits<float>::infinity();
	for(unsigned int i = 0; i < currentAtoms.size(); i++) {
		unsigned int index;
		index = currentAtoms[i]*11+variable;
		if(rawData[index] > max)
				   max = rawData[index];
		if(rawData[index] < min)
			min = rawData[index];
	}
	double range = max - min;
	
	for(int i = 0; i < 40; i++)
		histogram.push_back(0);
	
	for(unsigned int i = 0; i < currentAtoms.size(); i++) {
		unsigned int index;
		index = currentAtoms[i]*11+variable;
		double percentage = (rawData[index]-min)/range;
		
		percentage = percentage*40;
		int perint = (int)percentage;
		if(perint > 39)
			perint = 39;
		histogram[perint]+=1;
	}
	for(int i = 0; i < 40; i++) {
		histogram[i] = (histogram[i]/(float)currentAtoms.size()) * 40.;
	}
	
	std::cout << "min:" << min << " max" << max << std::endl;
	std::cout << "var:" << variable << std::endl;
	float offset = -min;
	float scale = 1./(max - min);
	float clipscale = 1./(topClip - bottomClip);
	int r, g, b,a;
	a = 255;
	for(unsigned int i = 0; i < currentAtoms.size(); i++) {
		if(timeIndex[currentAtoms[i]] <= lastTime) {
			unsigned int index;
			index = currentAtoms[i]*11+variable;
			float rawNum = (rawData[index]+offset)*scale;
			float colorVal = (rawNum -bottomClip)*clipscale;
			if(colorVal < 0) {
				colorVal = 0;
			}
			if(colorVal > 1) {
				colorVal = 1;
			}
			r = colorVal *255;
			g = 255;
			b = colorVal *255;
			int h, s, v;
			h = 0;
			s = 225;
			v = 150;
			h = colorVal*255;
			HSVtoRGB( &r, &g,&b, h, s, v );
			osg::Vec3f pos;
			pos.x() = rawData[currentAtoms[i]*11];
			pos.y() = rawData[currentAtoms[i]*11+1];
			pos.z() = rawData[currentAtoms[i]*11+2];
			vertices->push_back(pos);
			colours->push_back(osg::Vec4ub(r,g,b,a));
		}
	}
	
	for(unsigned int i = 0; i < 40; i++) {
		double perc = ((double)i)/40.0;
		double colorVal = (perc-bottomClip)*clipscale;
		if(colorVal < 0) {
			colorVal = 0;
		}
		if(colorVal > 1) {
			colorVal = 1;
		}
		
		r = colorVal *255;
		g = 255;
		b = colorVal *255;
		
		int h, s, v;
		h = 0;
		s = 225;
		v = 150;
		h = colorVal*255;
		HSVtoRGB( &r, &g,&b, h, s, v );
		
		
		rr.push_back(r);
		bb.push_back(b);
		gg.push_back(g);
	}
	std::cout << vertices->size() << "pushed to the card" << std::endl;
	atomProbeGeometry->setColorArray(colours);
	atomProbeGeometry->setVertexArray(vertices);
	atomProbeGeometry->setPrimitiveSet(0,new osg::DrawArrays(GL_POINTS,0,vertices->size()));
	m_HistogramDataSignal(rr,gg,bb, histogram);
	//reMask(currentMaskVariable, currentMaskBottom, currentMaskTop);

}

void AtomProbeGraphicalPlugin::reMask(int variable, float bottomClip, float topClip) {
	
	
	std::vector<unsigned int> atomsTemp;
	histogram.clear();
	float max = -std::numeric_limits<float>::infinity();
	float min = std::numeric_limits<float>::infinity();
	for(unsigned int i = 0; i < currentAtoms.size(); i++) {
		unsigned int index;
		index = currentAtoms[i]*11+variable;
		if(rawData[index] > max)
			max = rawData[index];
		if(rawData[index] < min)
			min = rawData[index];
	}
	double range = max - min;
	histogram.clear();
	for(int i = 0; i < 40; i++)
		histogram.push_back(0);
	
	for(unsigned int i = 0; i < currentAtoms.size(); i++) {
		unsigned int index;
		index = currentAtoms[i]*11+variable;
		double percentage = (rawData[index]-min)/range;
		
		percentage = percentage*40;
		int perint = (int)percentage;
		if(perint > 39)
			perint = 39;
		histogram[perint]+=1;
	}
	for(int i = 0; i < 40; i++) {
		histogram[i] = (histogram[i]/(float)currentAtoms.size()) * 40.;
	}
	
	
	
	float offset = -min;
	float scale = 1./(max - min);
	float clipscale = 1./(topClip - bottomClip);
	coloursOnCard->clear();
	//vertices->clear();
	int r, g, b,a;
	a = 255;
	for(unsigned int i = 0; i < currentAtoms.size(); i++) {
		unsigned int index;
		index = currentAtoms[i]*11+variable;
		float rawNum = (rawData[index]+offset)*scale;
		float colorVal = (rawNum -bottomClip)*clipscale;
		if(colorVal < 0) {
			continue;
		}
		if(colorVal > 1) {
			continue;
		}
		r = colorVal *255;
		g = 255;
		b = colorVal *255;
		osg::Vec3f pos;
		pos.x() = rawData[currentAtoms[i]*11];
		pos.y() = rawData[currentAtoms[i]*11+1];
		pos.z() = rawData[currentAtoms[i]*11+2];
		atomsTemp.push_back(i);
		//vertices->push_back(pos);
		//coloursOnCard->push_back((*colours)[i]);
		//if(i < 100) {
		/*std::cout << i << std::endl;
		 std::cout << "rawData;" << rawData[index] << std::endl;
		 std::cout << "rawnum:" << rawNum << std::endl;
		 std::cout << "colorVal:" << colorVal << std::endl;*/
		//}
		
	}
	//atomProbeGeometry->setColorArray(coloursOnCard);
	//atomProbeGeometry->setVertexArray(vertices);
	//atomProbeGeometry->setPrimitiveSet(0,new osg::DrawArrays(GL_POINTS,0,vertices->size()));
	m_MaskHistogramDataSignal(histogram);
	currentAtoms = atomsTemp;
	

}

void AtomProbeGraphicalPlugin::hitColorization(std::vector<int> rr, std::vector<int> gg, std::vector<int> bb, std::vector<bool> enabled) {
	std::vector<unsigned int> atomsTemp;
	int r, g, b,a;
	a = 255;
	vertices->clear();
	colours->clear();
	int lastIndex = rr.size() -1;
	for(unsigned int i = 0; i < currentAtoms.size(); i++) {
		int colorIndex;
		if(timeIndex[currentAtoms[i]] <= lastTime) {
			if(rawData[currentAtoms[i]*11+10] > lastIndex)
				colorIndex = lastIndex;
			else
				colorIndex = rawData[currentAtoms[i]*11+10];
			int index = currentAtoms[i]*11+10;
			//		h = colorVal*255;
			//HSVtoRGB( &r, &g,&b, h, s, v );
			r = rr[colorIndex];
			b = bb[colorIndex];
			g = gg[colorIndex];
			osg::Vec3f pos;
			pos.x() = rawData[currentAtoms[i]*11];
			pos.y() = rawData[currentAtoms[i]*11+1];
			pos.z() = rawData[currentAtoms[i]*11+2];
			vertices->push_back(pos);
			colours->push_back(osg::Vec4ub(r,g,b,a));
		}
	}
	atomProbeGeometry->setColorArray(colours);
	atomProbeGeometry->setVertexArray(vertices);
	atomProbeGeometry->setPrimitiveSet(0,new osg::DrawArrays(GL_POINTS,0,vertices->size()));
}

void AtomProbeGraphicalPlugin::hitMasking(std::vector<bool> enabled) {
	std::cout << enabled.size() << " different masks" << std::endl;
	int lastIndex = enabled.size() -1;
	std::vector<unsigned int> atomsTemp;
	for(unsigned int i = 0; i < currentAtoms.size(); i++) {
		int colorIndex;
		if(rawData[currentAtoms[i]*11+10] > lastIndex)
			colorIndex = lastIndex;
		else
			colorIndex = rawData[currentAtoms[i]*11+10];
		if(enabled[colorIndex])
			atomsTemp.push_back(currentAtoms[i]);
		
	}
	currentAtoms = atomsTemp;
}



