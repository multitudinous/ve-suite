/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
 *
 * Original Development Team:
 *    - ISU's Thermal Systems Virtual Engineering Group,
 *      Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *    - Reaction Engineering International, www.reaction-eng.com
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
 * Version:         $Rev$
 * Author:          $Author$
 * Id:                $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/Switch.h>

#include <ves/xplorer/Debug.h>

#include <ves/xplorer/util/fileIO.h>

#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>
#include <ves/xplorer/scenegraph/nurbs/KnotVector.h>
#include <ves/xplorer/scenegraph/nurbs/NCurve.h>
#include <ves/xplorer/scenegraph/nurbs/NSurface.h>
#include <ves/xplorer/scenegraph/nurbs/NURBS.h>
#include <ves/xplorer/scenegraph/nurbs/util/OCCNURBSFileReader.h>

// --- OSG Includes --- //
#include <osg/Fog>
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Switch>
#include <osg/Sequence>
#include <osg/MatrixTransform>
#include <osg/State>
#include <osg/StateSet>
#include <osg/StateAttribute>
#include <osg/Material>
#include <osg/BlendFunc>
#include <osg/Array>
#include <osg/Depth>
#include <osg/LOD>
#include <osg/ShadeModel>
#include <osg/LightModel>
#include <osg/BlendColor>

#include <osg/io_utils>
#include <osg/ComputeBoundsVisitor>
#include <osg/BoundingBox>

#include <osgUtil/SmoothingVisitor>
#include <osgUtil/Optimizer>

#include <osgDB/ReadFile>
#include <osgDB/Registry>
#include <osgDB/ReaderWriter>
#include <osgDB/FileUtils>
#include <osgDB/FileNameUtils>

#include <osg/Version>

#include <ves/xplorer/scenegraph/SceneManager.h>
#if ((OSG_VERSION_MAJOR>=2) && (OSG_VERSION_MINOR>=4))
#include <osg/OcclusionQueryNode>
#include <ves/xplorer/scenegraph/util/OcclusionQueryVisitor.h>
#else
#include <osgOQ/OcclusionQueryNode.h>
#include <osgOQ/OcclusionQueryVisitor.h>
#endif
// --- C/C++ Libraries --- //
#include <cctype>
#include <sstream>
#include <istream>
#include <string>
#include <cctype>

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
CADEntityHelper::CADEntityHelper()
{
    mIsSTLFile = false;
}
////////////////////////////////////////////////////////////////////////////////
CADEntityHelper::CADEntityHelper( const CADEntityHelper& input )
{
    if( !input.m_cadNode.valid() )
    {
        std::cerr << "ERROR : CADEntityHelper::CADEntityHelper not a valid node"
        << std::endl;
        return;
    }

    ///We deep copy nodes so that picking is accurate
    ///and so that physics will work properly in the future
    if( input.m_cadNode->asGroup() )
    {
        m_cadNode = new osg::Group( *input.m_cadNode->asGroup(),
           osg::CopyOp::DEEP_COPY_NODES | 
           osg::CopyOp::DEEP_COPY_STATESETS | 
           osg::CopyOp::DEEP_COPY_STATEATTRIBUTES );
    }
    else if( dynamic_cast< osg::Geode* >( input.m_cadNode.get() ) )
    {
        m_cadNode = new osg::Geode( *static_cast< osg::Geode* >(
                                        input.m_cadNode.get() ),
           osg::CopyOp::DEEP_COPY_NODES | 
           osg::CopyOp::DEEP_COPY_STATESETS | 
           osg::CopyOp::DEEP_COPY_STATEATTRIBUTES );
    }
    else
    {
        std::cout << "ERROR : Cast not present " << std::endl;
        std::cout << typeid( *input.m_cadNode.get() ).name() << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
CADEntityHelper& CADEntityHelper::operator=( const CADEntityHelper& input )
{
    if( this != &input )
    {
#ifdef _OSG
        //Recreate the node
        m_cadNode = input.m_cadNode;
#elif _OPENSG
#endif
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
CADEntityHelper::~CADEntityHelper()
{
    //If neccesary
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::SetNode( osg::Node* node )
{
    m_cadNode = node;
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* CADEntityHelper::GetNode()
{
    return m_cadNode.get();
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::SetName( const std::string& name )
{
    if( GetNode() )
    {
        GetNode()->setName( name.c_str() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::ToggleDisplay( const bool onOff )
{
    std::string value = ( onOff == true ) ? "ON" : "OFF";

    ToggleDisplay( value );
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::ToggleDisplay( const std::string& onOff )
{
    if( !GetNode() )
    {
        return;
    }

    if( onOff == "ON" )
    {
        GetNode()->setNodeMask( 1 );
    }
    else if( onOff == "OFF" )
    {
        GetNode()->setNodeMask( 0 );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::LoadFile( const std::string& filename,
                                const bool isStream, const bool occlude )
{
    if( strstr( filename.c_str(), ".stl" ) ||
            strstr( filename.c_str(), ".stla" ) )
    {
        mIsSTLFile = true;
    }

    osg::ref_ptr< osg::Node > tempCADNode;
    if( !isStream )
    {
        if( osgDB::getLowerCaseFileExtension( filename ) == "osg" )
        {
            osgDB::ReaderWriter *rw = osgDB::Registry::instance()->
                                      getReaderWriterForExtension(
                                          osgDB::getLowerCaseFileExtension( filename ) );
            if( !rw )
            {
                std::cerr << "Error: could not find a suitable " 
                    << "reader/writer to load the specified file" << std::endl;
                return;
            }

            //osgDB::findDataFile(
            std::auto_ptr< progbuf > pb( new progbuf( filename ) );
            if( !pb->is_open() )
            {
                std::cerr << "Error: could not open file `"
                    << filename << "'" << std::endl;
                return;
            }

            std::cout << "Progress: ";

            std::istream mis( pb.get() );
            osgDB::ReaderWriter::ReadResult rr = rw->readNode( mis );

            std::cout << std::endl;

            tempCADNode = rr.getNode();
            if( !tempCADNode.valid() )
            {
                std::cerr << "Error: could not load file `"
                << filename << "'" << std::endl;
            }
        }
        else if( osgDB::getLowerCaseFileExtension( filename ) == "ven" )
        {
            ///Get directory to look for txt files
            std::string venDirectory;
            std::ifstream inputDirectory( filename.c_str() );
            inputDirectory >> venDirectory;
            ///Load in txt files
            /*boost::filesystem::path fullPathFilename =
                        boost::filesystem::system_complete(
                        boost::filesystem::path( filename.c_str(),
                        boost::filesystem::native) );
                    fullPathFilename.*/
            boost::filesystem::path fullPathFilename =
                boost::filesystem::system_complete(
                    boost::filesystem::path( venDirectory.c_str(),
                                             boost::filesystem::native ) );
            //std::cout << venDirectory << " "
            //<< fullPathFilename.native_file_string() << std::endl;
            tempCADNode =
                parseOCCNURBSFile( fullPathFilename.native_file_string() );
            ///get osg node
        }
        else
        {
            boost::filesystem::path fullPathFilename =
                boost::filesystem::system_complete(
                    boost::filesystem::path( filename.c_str(),
                                             boost::filesystem::native ) );
            std::string fullPath;
            if( boost::filesystem::exists( fullPathFilename ) )
            {
                fullPath = fullPathFilename.native_file_string();
            }
            tempCADNode = osgDB::readNodeFile( fullPath );
            ///Check for cached file when reloading file with ves file
            //osgDB::fileExists( destFile );
            //osgDB::Registry::instance()->getReaderWriterForExtension( "osg" );
            //osgDB::getLowerCaseFileExtension(filename)
            //std::string shortName = osgDB::getNameLessExtension( fName );
            //ext = osgDB::getFileExtension( shortName );
            if( !tempCADNode.valid() )
            {
                std::string ptFileTest = ComputeIntermediateFileNameAndPath( filename );
                if( !ptFileTest.empty() )
                {
                    fullPath = ptFileTest;
                }
                tempCADNode = osgDB::readNodeFile( fullPath );
            }
        }
    }
    else
    {
        std::istringstream textNodeStream( filename );
        tempCADNode = osgDB::Registry::instance()->
                      getReaderWriterForExtension( "osg" )->
                      readNode( textNodeStream ).getNode();
    }

    if( !tempCADNode.valid() )
    {
        std::cerr << "|\tERROR (CADEntityHelper::LoadFile) loading file name: "
            << filename << std::endl;
        return;
    }

    if( mIsSTLFile )
    {
        osg::ref_ptr< osg::LightModel > lightModel;
        lightModel = new osg::LightModel;
        lightModel->setTwoSided( true );
        tempCADNode->getOrCreateStateSet()->setAttributeAndModes(
            lightModel.get(), 
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        
       /* osg::ref_ptr< osg::BlendColor > bc = new osg::BlendColor();
        bc->setConstantColor( osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        tempCADNode->getOrCreateStateSet()->setAttributeAndModes( 
            bc.get(), 
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );*/
        
        /*osg::ref_ptr< osg::StateSet > stateset = tempCADNode->getOrCreateStateSet();
        
        osg::ref_ptr< osg::Material > material = new osg::Material();
        material->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.56862f, 0.56842f, 0.56842f, 1.0f ) );
        material->setColorMode( osg::Material::AMBIENT_AND_DIFFUSE );
        stateset->setAttribute( material.get(), osg::StateAttribute::ON );*/
        
    }

    //Run the optimizer to improve performance
    {
        osgUtil::Optimizer graphOpti;
        graphOpti.optimize( tempCADNode.get(), 
                           osgUtil::Optimizer::FLATTEN_STATIC_TRANSFORMS |
                           osgUtil::Optimizer::REMOVE_REDUNDANT_NODES |
                           osgUtil::Optimizer::REMOVE_LOADED_PROXY_NODES |
                           osgUtil::Optimizer::COMBINE_ADJACENT_LODS |
                           //This one can cause problems with opacity settings
                           //osgUtil::Optimizer::SHARE_DUPLICATE_STATE |
                           osgUtil::Optimizer::MERGE_GEOMETRY |
                           osgUtil::Optimizer::CHECK_GEOMETRY |
                           //This one causes problems when creating physics
                           //meshes for osgBullet
                           //osgUtil::Optimizer::SPATIALIZE_GROUPS |
                           osgUtil::Optimizer::OPTIMIZE_TEXTURE_SETTINGS |
                           osgUtil::Optimizer::MERGE_GEODES |
                           osgUtil::Optimizer::STATIC_OBJECT_DETECTION );
    }

#if ((OSG_VERSION_MAJOR>=2) && (OSG_VERSION_MINOR>=4))
    osg::ref_ptr< osg::OcclusionQueryNode > root;
    root = dynamic_cast< osg::OcclusionQueryNode* >( tempCADNode.get() );
#else
    osg::ref_ptr< osgOQ::OcclusionQueryNode > root;
    root = dynamic_cast< osgOQ::OcclusionQueryNode* >( tempCADNode.get() );
#endif
    if( !root.valid() && occlude )
    {
        osg::ref_ptr< osg::Group > tempGroup = new osg::Group();
        tempGroup->addChild( tempCADNode.get() );

        osgOQ::OcclusionQueryNonFlatVisitor oqv;
        //Specify the vertex count threshold for performing 
        // occlusion query tests.
        //Settings others use are:
        //Fairly lax culling
        //occlusionThreshold = 5000
        //visibilityThreshold = 250
        //Fairly aggressive culling
        //occlusionThreshold = 2500
        //visibilityThreshold = 500
        // If the child geometry has less than the specified number
        //   of vertices, don't perform occlusion query testing (it's
        //   an occluder). Otherwise, perform occlusion query testing
        //   (it's an occludee).
        oqv.setOccluderThreshold( 2500 );
        tempGroup->accept( oqv );
        //Setup the number frames to skip
        osgOQ::QueryFrameCountVisitor queryFrameVisitor( 2 );
        tempGroup->accept( queryFrameVisitor );
        // If the occlusion query test indicates that the number of
        //   visible pixels is greater than this value, render the
        //   child geometry. Otherwise, don't render and continue to
        //   test for visibility in future frames.
        osgOQ::VisibilityThresholdVisitor visibilityThresholdVisitor( 250 );
        tempGroup->accept( visibilityThresholdVisitor );

        m_cadNode = tempGroup.get();
    }
    else
    {
        m_cadNode = tempCADNode;
    }

    if( !isStream )
    {
        m_cadNode->setName( filename.c_str() );
    }
    else
    {
        std::string nodeName = m_cadNode->getName();
        if( nodeName.empty() )
        {
            m_cadNode->setName( "NULL_FILENAME" );
        }
    }
    
    //Set per vertex lighting on all files that are loaded
    //osgUtil::SmoothingVisitor smoother;
    //m_cadNode->accept( smoother );
    
    {
        osg::ComputeBoundsVisitor cbbv( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN );
        m_cadNode->accept(cbbv);
        osg::BoundingBox bb = cbbv.getBoundingBox();
        std::cout << "|\tBounding Box Info" << std::endl 
            << "|\tCenter " << bb.center() << std::endl
            << "|\tRadius " << bb.radius() << std::endl
            << "|\tMin " << bb._min << std::endl
            << "|\tMax " << bb._max << std::endl;        
    }    
}
////////////////////////////////////////////////////////////////////////////////
std::string CADEntityHelper::
ComputeIntermediateFileNameAndPath( const std::string& srcFile ) const
{
    std::string intermediateFileNameAndPath = "";
    std::string inFile;
    inFile = srcFile;

    //then no filename was given by the user, so we'll
    //use the same filename as the file that was imported
    int indexOfFirstDot = ( int )( inFile.find_last_of( '.' ) );
    std::string tempExt( inFile.begin() + indexOfFirstDot + 1, inFile.end() );
    //See if the extension is only 1 char which would mean a ProE file
    if( tempExt.size() == 1 )
    {
        //double check and make sure the extension is a digit
        if( std::isdigit( tempExt.at( 0 ) ) )
        {
            //if so then grab the real index before the 3 space extension
            indexOfFirstDot =
                ( int )( inFile.find_last_of( '.', indexOfFirstDot - 1 ) );
        }
    }

    if( indexOfFirstDot > 0 )
    {
        intermediateFileNameAndPath = inFile.substr( 0, indexOfFirstDot );
    }

    std::string objTest;
    objTest = intermediateFileNameAndPath + ".obj";
    if( osgDB::fileExists( objTest ) )
    {
        return objTest;
    }

    objTest = intermediateFileNameAndPath + ".flt";
    if( osgDB::fileExists( objTest ) )
    {
        return objTest;
    }

    objTest.clear();
    return objTest;
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* CADEntityHelper::parseOCCNURBSFile( const std::string& directory )
{
    std::vector< osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBS> > nurbsPatches;
    //std::string nurbsfile(argv[1]);
    std::vector< std::string > patchFiles =
        ves::xplorer::util::fileIO::GetFilesInDirectory( directory, ".txt" );
    size_t nPatches = patchFiles.size();
    ves::xplorer::scenegraph::nurbs::util::OCCNURBSFileReader patchReader;

    for( size_t i = 0; i < nPatches;i++ )
    {
        ves::xplorer::scenegraph::nurbs::NURBSSurface* surface = patchReader.ReadPatchFile( patchFiles.at( i ) );
        if( surface )
        {
            surface->SetInterpolationGridSize( 10, "U" );
            surface->SetInterpolationGridSize( 20, "V" );
            surface->Interpolate();

            osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBS> renderablePatch =
                new ves::xplorer::scenegraph::nurbs::NURBS( surface );
            nurbsPatches.push_back( renderablePatch.get() );
        }
        else
        {
            std::cout << "Could not open file: " << patchFiles.at( i ) << std::endl;
        }
    }

    if( nurbsPatches.size() )
    {
        m_venNode =
            new osg::PositionAttitudeTransform();
        for( size_t i = 0; i < nurbsPatches.size(); i++ )
        {
            m_venNode->addChild( nurbsPatches.at( i ).get() );
        }
        return m_venNode.get();
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
