/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdVEBaseClass.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_INTSTOVE_MOD_H
#define VE_INTSTOVE_MOD_H

#include <vtkActor.h>

#include <ves/xplorer/plugin/PluginBase.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/DCS.h>

#include <vector>

namespace ves
{
namespace xplorer
{
   class DataSet;

namespace scenegraph
{
   class cfdModuleGeometry;
   class Group;
   class Geode;
   class CADEntity;
}
}
}
/*
namespace VE_Xplorer
{
   class DataSet;
}
*/
class vtkDataSet;
class vtkUnstructuredGrid;
class vtkGlyph3D;

// Need to create or use this in our stuff
class VEIntStovemod: public ves::xplorer::plugin::PluginBase 
{
public:
    VEIntStovemod( void );
    ~VEIntStovemod( void );

    virtual void InitializeNode( ves::xplorer::scenegraph::DCS* );
    virtual void CreateCustomVizFeature( int );
    void BuildBaffleGeom( vtkUnstructuredGrid* );
    virtual void SetCurrentCommand(ves::open::xml::Command* command);
      virtual void PreFrameUpdate( void );
private:
    ///Load the stove dataset when needed
    void LoadStoveDataSet();
    ///dataset used internally
    vtkDataSet* m_stoveData;
    ///Create the contour plane for the dataset that is loaded
    void CreateContourPlane();
    ///Create the vector plane for the dataset that is loaded
    void CreateVectorPlane();
    ///The geode for the contour plane that is created
    osg::ref_ptr< osg::Geode > m_contourPlane;
    ///The geode for the vector plane that is created
    osg::ref_ptr< osg::Geode > m_vectorPlane;   

    ves::xplorer::scenegraph::Geode* _geode;

public:
    int setcount;
    char* _outFileName;
    vtkUnstructuredGrid* _dataSet;
    double scale[3], trans[3], rotate[3];   // pfDCS stuff
    double geomscale[3], geomtrans[3], geomrotate[3];   // pfDCS stuff
    float stlColor[3];
    int color;
    int transFlag;

    int numbaffles;
    std::vector<std::string> baffle1str;
    std::vector<std::string> baffle2str;
    std::vector<std::string> baffle3str;
    std::vector<std::string> baffle4str;
    std::vector<std::string> baffle5str;
    std::vector<std::string> baffle6str;
    std::vector<std::string> baffle7str;
    std::vector<double> baffle1;
    std::vector<double> baffle2;
    std::vector<double> baffle3;
    std::vector<double> baffle4;
    std::vector<double> baffle5;
    std::vector<double> baffle6;
    std::vector<double> baffle7;

    std::vector< double > baffleParams;
    double baffleNum;
    double startXPos;
    double startYPos;
    double direction;
    double length;
    double depth;

    unsigned int vectors;
    unsigned int contour;

    bool showVectors;
    bool showContour;

    ves::xplorer::scenegraph::CADEntity* baffleOne;
    ves::xplorer::scenegraph::CADEntity* baffleTwo;
    ves::xplorer::scenegraph::CADEntity* baffleThree;
    ves::xplorer::scenegraph::CADEntity* baffleFour;
    ves::xplorer::scenegraph::CADEntity* baffleFive;
    ves::xplorer::scenegraph::CADEntity* baffleSix;
    ves::xplorer::scenegraph::CADEntity* baffleSeven;

};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( VEIntStovemod )

#endif
   
