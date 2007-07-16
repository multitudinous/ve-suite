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
#include "VE_Xplorer/SceneGraph/DCS.h"

#include <vector>
#include <string>

#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"

#ifdef _OSG
    #include <osg/ref_ptr>
#elif _PERFORMER
#endif

namespace VE_Xplorer
{
    class cfdWriteTraverser;
    class cfdCommandArray;
}

namespace VE_SceneGraph
{
    class DCS;
    class CADEntityHelper;
}

//A reader that reads performer binary files
namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdTeacher : public cfdGlobalBase
{
public:
    ///Constructor
    cfdTeacher( std::string, VE_SceneGraph::DCS* );

    ///Destructor
    ~cfdTeacher();

    // compare VjObs_i commandArray with its child's value
    virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray )
        { return false;}

    // in future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand();

    ///Write out a performer bindary file
    ///\param SceneNode* and file name
    void writePFBFile( VE_SceneGraph::SceneNode* graph,std::string fileName);

    ///Get DCS 
    VE_SceneGraph::DCS* GetDCS( );
    ///Get a performer node output
    ///\param node ID
    VE_SceneGraph::CADEntityHelper* getpfNode( int );
    ///Set/Get number of fles
    ///\param i file name ID
    int getNumberOfFiles();
    std::string getFileName( int i );

    ///Clear the stored scenes
    void ClearStoredScenes();
    ///Switch the active scene
    ///\param whichScene The scene to display
    void LoadScene(unsigned int whichScene);
    ///Save out the scene
    void RecordScene();

private:
    osg::ref_ptr< VE_SceneGraph::DCS > dcs;
    osg::ref_ptr< VE_SceneGraph::DCS > _worldDCS;
    std::vector< VE_SceneGraph::CADEntityHelper* > node;  // array of nodes
    ///Sorted file filenames for stored scenes
    std::vector<std::string> pfbFileNames;
    std::string directory;
};
}
#endif   // CFD_TEACHER_H
