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
#ifndef CFD_TEACHER_H
#define CFD_TEACHER_H
/*!\file cfdTeacher.h
cfdTeacher API
*/
/*!\class VE_Xplorer::cfdTeacher
* 
*/
#include <ves/xplorer/scenegraph/DCS.h>

#include <vector>
#include <string>

#include <ves/xplorer/cfdGlobalBase.h>

#ifdef _OSG
    #include <osg/ref_ptr>
#elif _PERFORMER
#endif

namespace VE_Xplorer
{
    class cfdWriteTraverser;
    class cfdCommandArray;
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
namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdTeacher : public cfdGlobalBase
{
public:
    ///Constructor
    cfdTeacher( std::string, ves::xplorer::scenegraph::DCS* );

    ///Destructor
    virtual ~cfdTeacher();

    // compare VjObs_i commandArray with its child's value
    virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray )
        { return false;}

    // in future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand();

    ///Write out a performer bindary file
    ///\param SceneNode* and file name
    void writePFBFile( ves::xplorer::scenegraph::SceneNode* graph,std::string fileName);

    ///Get DCS 
    ves::xplorer::scenegraph::DCS* GetDCS( );
    ///Get a performer node output
    ///\param node ID
    ves::xplorer::scenegraph::CADEntityHelper* getpfNode( int );
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
    void LoadScene(unsigned int whichScene);
    ///Save out the scene
    void RecordScene();
    ///Reset Teacher so that the new application's scenes can be loaded
    void Reset();
    
private:
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > dcs;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > _worldDCS;
    //std::vector< ves::xplorer::scenegraph::CADEntityHelper* > node;  // array of nodes
    ves::xplorer::scenegraph::CADEntityHelper* m_currentScene;
    ///Sorted file filenames for stored scenes
    std::vector<std::string> pfbFileNames;
    std::string directory;
};
}
#endif   // CFD_TEACHER_H
