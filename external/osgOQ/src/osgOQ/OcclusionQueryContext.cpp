//
// Copyright (C) 2007 Skew Matrix Software LLC (http://www.skew-matrix.com)
//
// This library is open source and may be redistributed and/or modified under  
// the terms of the OpenSceneGraph Public License (OSGPL) version 0.0 or 
// (at your option) any later version.  The full license is in LICENSE file
// included with this distribution, and on the openscenegraph.org website.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
// OpenSceneGraph Public License for more details.
//

#include "osgOQ/OcclusionQueryContext.h"
#include "osgOQ/ConfigFileReader.h"
#include <osg/StateAttribute>
#include <osg/PolygonMode>
#include <osg/ColorMask>
#include <osg/PolygonOffset>
#include <osg/Depth>
#include <osg/Notify>
#include <osgDB/FileUtils>
#include <string>
#include <sstream>

using std::string;


namespace osgOQ {


// Define static member variables
std::string OcclusionQueryContext::_visibilityThresholdToken( "VisibilityThreshold" );
std::string OcclusionQueryContext::_occluderThresholdToken( "OccluderThreshold" );
std::string OcclusionQueryContext::_debugBoundingVolumeToken( "DebugBoundingVolume" );
std::string OcclusionQueryContext::_placementToken( "UseNonFlatQueryNodePlacement" );
std::string OcclusionQueryContext::_queryFrameCountToken( "QueryFrameCount" );

OcclusionQueryContext::OcclusionQueryContext()
  : _visThreshold( 500 ),
    _occluderThreshold( 5000 ),
	_debugBB( false ),
	_useNonFlat( true ),
	_nameIdx( 0 ),
	_statistics( true ),
	_numQueries( 0 ),
	_numPassed( 0 ),
	_numFailed( 0 ),
	_debugVerbosity( 0 )
{
    // Look for config file.
    // If found, parse it and set values in _oqc from it.
	ConfigFileReader configFileReader;
	{
		string envStr( "OSGOQ_CONFIG_FILE" );
		char* charPtr = getenv( envStr.c_str() );
		if (charPtr)
		{
			string configFile = string( charPtr );
			string fullName = osgDB::findDataFile( configFile );
			bool setSuccess( false );
			if (!fullName.empty())
				setSuccess = configFileReader.SetConfigFileName( fullName );
			if (!setSuccess)
				osg::notify(osg::NOTICE) << "osgOQ: Can't load " << envStr << ": \"" << fullName << "\"." << std::endl;

			// Check config file for OQC-related options
			if ( configFileReader.HasProperty( _visibilityThresholdToken ) )
			{
				string valueStr = configFileReader.GetValue( _visibilityThresholdToken );
				int value;
				if (ConfigFileReader::convertToInt( value, valueStr ))
					setVisibilityThreshold( value );
			}
			if ( configFileReader.HasProperty( _occluderThresholdToken ) )
			{
				string valueStr = configFileReader.GetValue( _occluderThresholdToken );
				int value;
				if (ConfigFileReader::convertToInt( value, valueStr ))
					setOccluderThreshold( value );
			}
			if ( configFileReader.HasProperty( _debugBoundingVolumeToken ) )
			{
				string valueStr = configFileReader.GetValue( _debugBoundingVolumeToken );
				bool value;
				if (ConfigFileReader::convertToBool( value, valueStr ))
					setDebugDisplay( value );
			}
			if ( configFileReader.HasProperty( _placementToken ) )
			{
				string valueStr = configFileReader.GetValue( _placementToken );
				bool value;
				if (ConfigFileReader::convertToBool( value, valueStr ))
					setNonFlatPlacement( value );
			}
			if ( configFileReader.HasProperty( _queryFrameCountToken ) )
			{
				string valueStr = configFileReader.GetValue( _queryFrameCountToken );
				int value;
				if (ConfigFileReader::convertToInt( value, valueStr ))
                {
                    if (value < 1)
                        value = 1;
					setQueryFrameCount( value );
                }
			}
		}
	}

	// Init StateSet for occlusion query geometry.
    {
        _state = new osg::StateSet;
		// TBD Possible bug, need to allow user to set render bin number.
		_state->setRenderBinDetails( 9, "RenderBin" );

		_state->setMode( GL_LIGHTING, osg::StateAttribute::OFF |
            osg::StateAttribute::PROTECTED);
        _state->setTextureMode( 0, GL_TEXTURE_2D, osg::StateAttribute::OFF |
            osg::StateAttribute::PROTECTED);
        _state->setMode( GL_CULL_FACE, osg::StateAttribute::ON |
            osg::StateAttribute::PROTECTED);

        osg::ColorMask* cm = new osg::ColorMask( false, false, false, false );
        _state->setAttributeAndModes( cm, osg::StateAttribute::ON |
            osg::StateAttribute::PROTECTED);
        osg::Depth* d = new osg::Depth( osg::Depth::LEQUAL, 0.f, 1.f, false );
        _state->setAttributeAndModes( d, osg::StateAttribute::ON |
            osg::StateAttribute::PROTECTED);
        osg::PolygonMode* pm = new osg::PolygonMode(
            osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::FILL );
        _state->setAttributeAndModes( pm, osg::StateAttribute::ON |
            osg::StateAttribute::PROTECTED);

		osg::PolygonOffset* po = new osg::PolygonOffset( -1., -1. );
        _state->setAttributeAndModes( po, osg::StateAttribute::ON |
            osg::StateAttribute::PROTECTED);
    }

	// Initialize StateSet for debug geometry
    {
        _debugState = new osg::StateSet;
		//_debugState->setRenderBinDetails( 0, "RenderBin" );
        _debugState->setMode( GL_LIGHTING, osg::StateAttribute::OFF |
            osg::StateAttribute::PROTECTED);
        _debugState->setTextureMode( 0, GL_TEXTURE_2D, osg::StateAttribute::OFF |
            osg::StateAttribute::PROTECTED);
        _debugState->setMode( GL_CULL_FACE, osg::StateAttribute::ON |
            osg::StateAttribute::PROTECTED);

        osg::PolygonMode* pm = new osg::PolygonMode(
            osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::LINE );
        _debugState->setAttributeAndModes( pm, osg::StateAttribute::ON |
            osg::StateAttribute::PROTECTED);

		osg::PolygonOffset* po = new osg::PolygonOffset( -1., -1. );
        _debugState->setAttributeAndModes( po, osg::StateAttribute::ON |
            osg::StateAttribute::PROTECTED);
    }
}
OcclusionQueryContext::~OcclusionQueryContext()
{
}

std::string
OcclusionQueryContext::getNextOQNName()
{
	std::ostringstream ostr;
	ostr << "OQNode_" << _nameIdx++;
	return ostr.str();
}

}
