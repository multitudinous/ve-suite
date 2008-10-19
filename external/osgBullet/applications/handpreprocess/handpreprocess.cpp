//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//

/*
  The data directory contains right and left hand models in
  FLT format, and equivalent models in IVE format. This code
  generates the IVE files from the FLT files.

  The hand model was created in Geo and exported to FLT.
  This application loads the source FLT files for the right
  and left hands, performs some operations on the loaded
  data, and exports them as IVE files for use by the
  HandNode class.

  Operations performed:
   * Key transform nodes are identified by description date
     and named appropriately for easy identification by
     HandNode
   * All descriptions are stripped.
   * Unnamed nodes are assigned names.

  This application should be ran when osgBullet is ported
  to new versions of OSG to keep the IVE files up to date.
*/

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osg/NodeVisitor>
#include <osgSim/DOFTransform>
#include <osgUtil/Optimizer>
#include <osg/StateSet>
#include <osg/Material>
#include <osg/CullFace>
#include <osg/Texture2D>
#include <osg/TexEnv>
#include <osg/Image>

#include <map>
#include <string>
#include <sstream>


class HandPreProcess : public osg::NodeVisitor
{
public:
    HandPreProcess()
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
        _nodeNum( 0 )
    {}
    ~HandPreProcess() {};

    void reset()
    {
        _nodeNum = 0;
        _unusedDOFs.clear();
        // Retain _nameMap
    }

    void removeUnusedDOFs()
    {
        osg::notify( osg::NOTICE ) << "Removing " << _unusedDOFs.size() << " unused DOFs." << std::endl;

        osg::NodeList::iterator nit;
        for ( nit = _unusedDOFs.begin(); nit != _unusedDOFs.end(); nit++ )
        {
            osg::Transform* xform = (*nit)->asTransform();
            if (xform->getNumParents() > 1)
                osg::notify( osg::WARN ) << "** didn't expect multiple parents." << std::endl;
            if (xform->getNumParents() < 1)
                continue;
            osg::Group* parent = xform->getParent( 0 );

            unsigned int idx;
            for ( idx = 0; idx < xform->getNumChildren(); idx++ )
                parent->addChild( xform->getChild( idx ) );
            parent->removeChild( xform );
        }
        _unusedDOFs.clear();
    }

    void apply( osg::Node& node )
    {
        node.setStateSet( NULL );
        node.setName( createUniqueNodeName() );
        node.getDescriptions().clear();
        node.setDataVariance( osg::Object::STATIC );
        traverse( node );
    }

    void apply( osg::Group& group )
    {
        group.setStateSet( NULL );
        group.setName( createUniqueNodeName() );
        group.getDescriptions().clear();
        group.setDataVariance( osg::Object::STATIC );
        traverse( group );
        removeChildGroups( group );
        removeChildEmptyGeodes( group );
    }

    void apply( osg::Transform& xform )
    {
        osg::notify( osg::NOTICE ) << xform.getName() << std::endl;

        osgSim::DOFTransform* doft = dynamic_cast< osgSim::DOFTransform* >( &xform );
        if ( doft )
        {
            const osg::Node::DescriptionList dl = xform.getDescriptions();
            bool found( false );
            NameMap::const_iterator it = _nameMap.begin();
            while( !found && (it != _nameMap.end()) )
            {
                if( find( it->first, dl ) )
                {
                    // Found the keyword in the description list.

                    // Avoid substrings, for example, "finger_06_j_02" and "finger_06_j_02_trans"
                    // Also avoid transforms outputting to Geo's "TempFloat".
                    if( ( !find( it->first + "_", dl ) ) && ( !find( "TempFloat", dl ) ) )
                    {
                        xform.setName( it->second );
                        xform.setDataVariance( osg::Object::DYNAMIC );
                        osg::notify( osg::NOTICE ) << "  Found " << it->first << std::endl;
                        osg::notify( osg::NOTICE ) << "    set name to " << xform.getName() << std::endl;
                        found = true;
                    }
                }
                it++;
            }
            if (!found)
            {
                // This is an unused DOF. Replace it with a Group.
                osg::notify( osg::NOTICE ) << "  Unused DOF." << std::endl;
                _unusedDOFs.push_back( doft );
                doft->setDataVariance( osg::Object::STATIC );
            }
        }
        else
        {
            osg::notify( osg::NOTICE ) << "  Not a DOF." << std::endl;
            _unusedDOFs.push_back( &xform );
            xform.setDataVariance( osg::Object::STATIC );
        }

        if (xform.getName().empty())
            xform.setName( createUniqueNodeName() );

        xform.setStateSet( NULL );
        xform.getDescriptions().clear();
        traverse( xform );
        removeChildGroups( xform );
        removeChildEmptyGeodes( xform );
    }

    void apply( osg::Geode& geode )
    {
        typedef std::list< unsigned int > IntsList;
        IntsList deleteList;

        unsigned int idx;
        for (idx=0; idx < geode.getNumDrawables(); idx++)
        {
            osg::Drawable* draw = geode.getDrawable( idx );
            draw->setStateSet( NULL );

            osg::Geometry* geom = dynamic_cast< osg::Geometry* >( draw );
            if (!geom)
            {
                osg::notify( osg::WARN ) << "Warning: Non-Geometry Drawable encountered." << std::endl;
                continue;
            }

            if (geom->getVertexArray()->getNumElements() <= 4)
            {
                osg::notify( osg::NOTICE ) << "Removing small Geometry." << std::endl;
                deleteList.push_front( idx );
            }
        }

        IntsList::iterator it = deleteList.begin();
        for( ; it != deleteList.end(); it++ )
            geode.removeDrawables( *it );

        geode.setStateSet( NULL );
        geode.setName( createUniqueNodeName() );
        geode.getDescriptions().clear();
        geode.setDataVariance( osg::Object::STATIC );
        traverse( geode );
    }

    void findAndChangeName( const std::string& from, const std::string& to )
    {
        _nameMap[ from ] = to;
    }

protected:
    void removeChildGroups( osg::Group& grp )
    {
        osg::NodeList addList;
        osg::NodeList removeList;

        unsigned int idx;
        for( idx = 0; idx < grp.getNumChildren(); idx++ )
        {
            osg::Node* node = grp.getChild( idx );
            if ( node->className() == std::string("Group") )
            {
                osg::Group* child = node->asGroup();
                unsigned int cidx;
                for( cidx = 0; cidx < child->getNumChildren(); cidx++ )
                    addList.push_back( child->getChild( cidx ) );
                removeList.push_back( child );
            }
        }

        osg::NodeList::iterator it;
        for( it = addList.begin(); it != addList.end(); it++ )
            grp.addChild( it->get() );

        osg::notify( osg::NOTICE ) << "Removed " << removeList.size() << " Groups." << std::endl;
        for( it = removeList.begin(); it != removeList.end(); it++ )
            grp.removeChild( it->get() );
    }

    void removeChildEmptyGeodes( osg::Group& grp )
    {
        osg::NodeList removeList;

        unsigned int idx;
        for( idx = 0; idx < grp.getNumChildren(); idx++ )
        {
            osg::Node* node = grp.getChild( idx );
            if ( node->className() == std::string("Geode") )
            {
                osg::Geode* geode = dynamic_cast< osg::Geode* >( node );
                if (geode->getNumDrawables() == 0)
                    removeList.push_back( geode );
            }
        }

        osg::notify( osg::NOTICE ) << "Removed " << removeList.size() << " Geodes." << std::endl;
        osg::NodeList::iterator it;
        for( it = removeList.begin(); it != removeList.end(); it++ )
            grp.removeChild( it->get() );
    }

    bool find( const std::string& target, const osg::Node::DescriptionList& dl )
    {
        unsigned int idx;
        for (idx=0; idx<dl.size(); idx++)
        {
            if (dl[ idx ].find( target ) != dl[ idx ].npos)
                return true;
        }
        return false;
    }

    std::string createUniqueNodeName()
    {
        std::ostringstream ostr;
        ostr << "n" << _nodeNum++;
        return( ostr.str() );
    }

    typedef std::map< std::string, std::string > NameMap;
    NameMap _nameMap;

    int _nodeNum;

    osg::NodeList _unusedDOFs;
};


int
main( int argc,
      char ** argv )
{
    osg::setNotifyLevel( osg::WARN   );


    // Prepare a suitable StateSet.
    osg::ref_ptr< osg::StateSet > state = new osg::StateSet;

    osg::Image* image = osgDB::readImageFile( "tex_hand_06.rgb" );
    osg::Texture2D* tex = new osg::Texture2D( image );
    state->setTextureAttributeAndModes( 0, tex );

    osg::TexEnv* te = new osg::TexEnv;
    state->setTextureAttributeAndModes( 0, te );

    osg::CullFace* cf = new osg::CullFace;
    state->setAttributeAndModes( cf );

    osg::Material* mat = new osg::Material;
    mat->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( .5, .5, .5, 1. ) );
    mat->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 1., 1., 1., 1. ) );
    state->setAttributeAndModes( mat );
    state->setMode( GL_LIGHTING, osg::StateAttribute::ON );


    // The original file lacks node names, but has keywords
    //   embedded in description text. Let's set some appropriate
    //   node names.
    // Configure the mapping of description text to node names.
    HandPreProcess hpp;
    hpp.findAndChangeName( "finger_01_j_01", "f1k1" );
    hpp.findAndChangeName( "finger_01_j_02", "f1k0" );
    hpp.findAndChangeName( "finger_01_j_02_trans", "f1trans" );
    hpp.findAndChangeName( "finger_02_j_01", "f2k1" );
    hpp.findAndChangeName( "finger_02_j_02", "f2k0" );
    hpp.findAndChangeName( "finger_02_j_02_trans", "f2trans" );
    hpp.findAndChangeName( "finger_03_j_01", "f3k1" );
    hpp.findAndChangeName( "finger_03_j_02", "f3k0" );
    hpp.findAndChangeName( "finger_03_j_02_trans", "f3trans" );
    hpp.findAndChangeName( "finger_04_j_01", "f4k1" );
    hpp.findAndChangeName( "finger_04_j_02", "f4k0" );
    hpp.findAndChangeName( "finger_04_j_02_trans", "f4trans" );
    hpp.findAndChangeName( "thumb_rt_j_01", "f0k1" );
    hpp.findAndChangeName( "thumb_rt_j_02", "f0k0" );
    hpp.findAndChangeName( "thumb_rt_j_trans", "f0trans" );
    
    hpp.findAndChangeName( "finger_05_j_01", "f1k1" );
    hpp.findAndChangeName( "finger_05_j_02", "f1k0" );
    hpp.findAndChangeName( "finger_05_j_02_trans", "f1trans" );
    hpp.findAndChangeName( "finger_06_j_01", "f2k1" );
    hpp.findAndChangeName( "finger_06_j_02", "f2k0" );
    hpp.findAndChangeName( "finger_06_j_02_trans", "f2trans" );
    hpp.findAndChangeName( "finger_07_j_01", "f3k1" );
    hpp.findAndChangeName( "finger_07_j_02", "f3k0" );
    hpp.findAndChangeName( "finger_07_j_02_trans", "f3trans" );
    hpp.findAndChangeName( "finger_08_j_01", "f4k1" );
    hpp.findAndChangeName( "finger_08_j_02", "f4k0" );
    hpp.findAndChangeName( "finger_08_j_02_trans", "f4trans" );
    hpp.findAndChangeName( "thumb_lft_j_01", "f0k1" );
    hpp.findAndChangeName( "thumb_lft_j_02", "f0k0" );
    hpp.findAndChangeName( "thumb_lft_j_trans", "f0trans" );

    osg::setNotifyLevel( osg::INFO );
    osgUtil::Optimizer optimizer;

    osg::ref_ptr< osgDB::ReaderWriter::Options > opt = new osgDB::ReaderWriter::Options;
    opt->setOptionString( "noTexturesInIVEFile" );

    std::string fileName( "handL.flt" );
    osg::notify( osg::ALWAYS ) << std::endl << "Processing " << fileName << std::endl;
    osg::ref_ptr< osg::Node > hand = osgDB::readNodeFile( fileName );
    if (!hand.valid())
    {
        osg::notify( osg::FATAL ) << "Can't load source file " << fileName << std::endl;
        return 1;
    }
    hand->accept( hpp );
    hpp.removeUnusedDOFs();
    hand->setStateSet( state.get() );
    optimizer.optimize( hand.get() );
    osgDB::writeNodeFile( *hand, "handL.ive", opt.get() );

    hpp.reset();
    optimizer.reset();

    fileName = "handR.flt";
    osg::notify( osg::ALWAYS ) << std::endl << "Processing " << fileName << std::endl;
    hand = osgDB::readNodeFile( fileName );
    if (!hand.valid())
    {
        osg::notify( osg::FATAL ) << "Can't load source file " << fileName << std::endl;
        return 1;
    }
    hand->accept( hpp );
    hpp.removeUnusedDOFs();
    hand->setStateSet( state.get() );
    optimizer.optimize( hand.get() );
    osgDB::writeNodeFile( *hand, "handR.ive", opt.get() );

    osg::notify( osg::ALWAYS ) << "Successful processing." << std::endl;

    return 0;
}
