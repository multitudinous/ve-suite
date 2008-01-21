/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
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
#ifdef _OSG
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

#include <osgDB/ReadFile>
#include <osgDB/Registry>
#include <osgDB/FileUtils>
#include <osgDB/ReaderWriter>
#include <osgDB/FileUtils>
#include <osgDB/FileNameUtils>
#elif _OPENSG
#endif

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <osgOQ/OcclusionQueryNode.h>
#include <osgOQ/OcclusionQueryVisitor.h>

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
    m_twoSidedLighting = false;
}
////////////////////////////////////////////////////////////////////////////////
CADEntityHelper::CADEntityHelper( const CADEntityHelper& input )
{
#ifdef _OSG
    if( !input.m_cadNode.valid() )
    {
        std::cerr << "ERROR : CADEntityHelper::CADEntityHelper not a valid node"
        << std::endl;
        return;
    }

    ///We deep copy nodes so that picking is accurate
    ///and so that physics will work properly in the future
    if( dynamic_cast< osgOQ::OcclusionQueryNode* >( input.m_cadNode.get() ) )
    {
        m_cadNode = new osgOQ::OcclusionQueryNode( 
            *static_cast< osgOQ::OcclusionQueryNode* >( 
            input.m_cadNode.get() ), osg::CopyOp::DEEP_COPY_NODES );
    }
    else if( input.m_cadNode->asGroup() )
    {
        m_cadNode = new osg::Group( *input.m_cadNode->asGroup(),
                                    osg::CopyOp::DEEP_COPY_NODES );
    }
    else if( dynamic_cast< osg::Geode* >( input.m_cadNode.get() ) )
    {
        m_cadNode = new osg::Geode( *static_cast< osg::Geode* >(
                                        input.m_cadNode.get() ), osg::CopyOp::DEEP_COPY_NODES );
    }
    else
    {
        std::cout << "ERROR : Cast not present " << std::endl;
        std::cout << typeid( *input.m_cadNode.get() ).name() << std::endl;
    }

#elif _OPENSG
#endif
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
#ifdef _OSG
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
#ifdef _OSG
void CADEntityHelper::SetNode( osg::Node* node )
#elif _OPENSG
#endif
{
#ifdef _OSG
    m_cadNode = node;
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
#ifdef _OSG
osg::Node* CADEntityHelper::GetNode()
#elif _OPENSG
#endif
{
#ifdef _OSG
    return m_cadNode.get();
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::SetName( std::string name )
{
    if( GetNode() )
    {
        GetNode()->setName( name.c_str() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::ToggleDisplay( bool onOff )
{
    std::string value = ( onOff == true ) ? "ON" : "OFF";

    ToggleDisplay( value );
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::ToggleDisplay( std::string onOff )
{
    if( !GetNode() )
    {
        return;
    }

    if( onOff == "ON" )
    {
#ifdef _OSG
        GetNode()->setNodeMask( 1 );
#elif _OPENSG
#endif
    }
    else if( onOff == "OFF" )
    {
#ifdef _OSG
        GetNode()->setNodeMask( 0 );
#elif _OPENSG
#endif
    }
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::LoadFile( std::string filename,
                                bool isStream, bool occlude )
{
    if( strstr( filename.c_str(), ".stl" ) ||
            strstr( filename.c_str(), ".stla" ) )
    {
        m_twoSidedLighting = true;
    }

#ifdef _OSG
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
                std::cerr << "Error: could not find a suitable " <<
                "reader/writer to load the specified file" << std::endl;
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

    if( m_twoSidedLighting )
    {
        osg::ref_ptr< osg::LightModel > lightModel;
        lightModel = new osg::LightModel;
        lightModel->setTwoSided( true );
        tempCADNode->getOrCreateStateSet()->setAttributeAndModes(
            lightModel.get(), osg::StateAttribute::ON );
    }

#elif _OPENSG
    std::cout << " Error:LoadFile !!! " << std::endl;
    exit( 1 );
#endif

    osg::ref_ptr< osgOQ::OcclusionQueryNode > root;
    root = dynamic_cast< osgOQ::OcclusionQueryNode* >( tempCADNode.get() );
    if( !root.valid() && occlude )
    {
        osg::ref_ptr< osg::Group > tempGroup = new osg::Group();
        tempGroup->addChild( tempCADNode.get() );

        osgOQ::OcclusionQueryNonFlatVisitor oqv;
        //Specify the vertex count threshold for performing 
        // occlusion query tests.
        oqv.setOccluderThreshold( 2500 );
        tempGroup->accept( oqv );
        //Setup the number frames to skip
        osgOQ::QueryFrameCountVisitor queryFrameVisitor( 3 );
        tempGroup->accept( queryFrameVisitor );
        //Setup the number frames to skip
        osgOQ::VisibilityThresholdVisitor visibilityThresholdVisitor( 500 );
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
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::AddOccluderNodes()
{
    osg::ref_ptr< osgOQ::OcclusionQueryNode > root;
    root = dynamic_cast< osgOQ::OcclusionQueryNode* >( m_cadNode.get() );
    if( !root.valid() && ( m_cadNode->getNumParents() > 0 ) )
    {
        osgOQ::OcclusionQueryNonFlatVisitor oqv;
        //Specify the vertex count threshold for performing 
        // occlusion query tests.
        oqv.setOccluderThreshold( 2500 );
        m_cadNode->accept( oqv );
        //Setup the number frames to skip
        osgOQ::QueryFrameCountVisitor queryFrameVisitor( 3 );
        m_cadNode->accept( queryFrameVisitor );
        //Setup the number frames to skip
        osgOQ::VisibilityThresholdVisitor visibilityThresholdVisitor( 500 );
        m_cadNode->accept( visibilityThresholdVisitor );
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
osg::Node* CADEntityHelper::parseOCCNURBSFile( std::string directory )
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
