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
#ifndef CFD_TEACHER_H
#define CFD_TEACHER_H
/*!\file cfdTeacher.h
cfdTeacher API
*/
/*!\class ves::xplorer::cfdTeacher
*
*/
#include <ves/xplorer/scenegraph/DCS.h>

#include <vector>
#include <string>

#include <ves/xplorer/GlobalBase.h>

#include <osg/ref_ptr>

namespace ves
{
namespace xplorer
{
class cfdWriteTraverser;
}
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;
class CADEntityHelper;
}
}
}

//A reader that reads performer binary files
namespace ves
{
namespace xplorer
{
class VE_XPLORER_EXPORTS cfdTeacher : public GlobalBase
{
public:
    ///Constructor
    cfdTeacher( std::string, osg::Group* );

    ///Destructor
    virtual ~cfdTeacher();

    // in future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand();

    ///Write out a performer bindary file
    ///\param SceneNode* and file name
    void writePFBFile( osg::Node* graph, std::string fileName );

    ///Get DCS
    ves::xplorer::scenegraph::DCS* GetDCS( );
    ///Get a performer node output
    ///\param node ID
    ves::xplorer::scenegraph::CADEntityHelper* GetCurrentLoadedScene( int );
    ///Set/Get number of fles
    ///\param i file name ID
    int getNumberOfFiles();
    ///The the filename of the i'th file
    ///\param i The i'th filename to be returned
    std::string getFileName( int i );
    ///Clear the stored scenes
    void ClearStoredScenes();
    ///Switch the active scene
    ///\param whichScene The scene to display
    void LoadScene( unsigned int whichScene );
    ///Save out the scene
    void RecordScene();
    ///Reset Teacher so that the new application's scenes can be loaded
    void Reset();

private:
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > dcs;
    osg::ref_ptr< osg::Group > mModelRoot;
    ves::xplorer::scenegraph::CADEntityHelper* m_currentScene;
    ///Sorted file filenames for stored scenes
    std::vector<std::string> pfbFileNames;
    std::string directory;
};
}
}
#endif   // CFD_TEACHER_H
