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
#ifndef CAD_ENTITY_HELPER_H
#define CAD_ENTITY_HELPER_H

/*!\file CADEntityHelper.h
*/

/*!\class ves::xplorer::scenegraph::CADEntityHelper
*Class to assist CADEntity
*/

/*!\namespace ves::xplorer::scenegraph
*
*/

// --- C/C++ Libraries --- //
#include <iostream>
#include <fstream>
#include <memory>

template< typename Elem, typename Tr = std::char_traits< Elem > >

class progress_streambuf: public std::basic_filebuf< Elem, Tr >
{
public:
    typedef std::basic_filebuf< Elem, Tr > base_type;

    explicit progress_streambuf( const std::string &filename )
            :
            base_type(),
            count_( 0 ),
            prev_perc_( 0 )
    {
        if( base_type::open( filename.c_str(), std::ios_base::in | std::ios_base::binary ) )
        {
            size_ = static_cast< int >( std::streambuf::pubseekoff( 0, std::ios_base::end, std::ios_base::in ) );
            std::streambuf::pubseekoff( 0, std::ios_base::beg, std::ios_base::in );
        }
    }

protected:
    virtual typename std::basic_filebuf< Elem, Tr >::int_type uflow()
    {
        typename std::basic_filebuf< Elem, Tr >::int_type v = base_type::uflow();
        count_ += std::streambuf::egptr() - std::streambuf::gptr();
        int p = count_ * 40 / size_;
        if( p > prev_perc_ )
        {
            std::cout << "*";
            prev_perc_ = p;
        }

        return v;
    }

private:
    int count_;
    int size_;
    int prev_perc_;
};

typedef progress_streambuf< char > progbuf;

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/Node>
#include <osg/PositionAttitudeTransform>
#include <osg/ref_ptr>

namespace osg
{
class Fog;
class LightModel;
}
#elif _OPENSG
#endif

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class VE_SCENEGRAPH_EXPORTS CADEntityHelper
{
public:
    ///Base Constructor
    CADEntityHelper();

    ///Copy constructor
    CADEntityHelper( const CADEntityHelper& );

    ///Destructor
    ~CADEntityHelper();

    ///Equal operator
    CADEntityHelper& operator=( const CADEntityHelper& );

    ///Set the name of the CADEntityHelper
    ///\param name The name of CADEntityHelper
    void SetName( std::string name );

    ///Toggle the display of this CADEntityHelper on/off
    ///Valid values are:
    ///ON == display this CADEntityHelper
    ///OFF == hide this CADEntityHelper
    ///\param onOff Turn on/off rendering of this CADEntityHelper
    void ToggleDisplay( std::string onOff );

    ///Toggle the display of this CADEntityHelper on/off
    ///\param onOff Turn on/off rendering of this CADEntityHelper
    void ToggleDisplay( bool onOff );

    ///Add occluder nodes
    ///Must call after this node has a parent
    void AddOccluderNodes();

#ifdef _OSG
    ///Set the node of CADEntityHelper
    ///\param node An osg::Node pointer
    void SetNode( osg::Node* node );

    ///Return the node of CADEntityHelper
    osg::Node* GetNode();
#elif _OPENSG
#endif

    ///Load a geometry file
    ///\param filename The name of the file to be loaded
    ///\param isStream Is the file a stream
    ///\param occlude Occlude the geom with osgOQ if desired
    void LoadFile( std::string filename, bool isStream = false,
                   bool occlude = false );
    ///Used for working with PolyTrans
    std::string ComputeIntermediateFileNameAndPath(
        const std::string& srcFile ) const;

    ///Process a ven file
    ///\param directory Directory with txt patch files
    ///\return The node for the surface
    osg::Node* parseOCCNURBSFile( std::string directory );

private:
#ifdef _OSG
    ///Node representing the loaded in geometry file
    osg::ref_ptr< osg::Node > m_cadNode;
    ///group node for ven
    osg::ref_ptr< osg::PositionAttitudeTransform > m_venNode;
#elif _OPENSG
#endif
    ///The current state of two sided lighting for the node
    bool m_twoSidedLighting;

};
}
}
}

#endif //CAD_ENTITY_HELPER_H
