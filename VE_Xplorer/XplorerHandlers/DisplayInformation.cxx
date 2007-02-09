#include "VE_Xplorer/XplorerHandlers/DisplayInformation.h"

#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
#include "VE_Xplorer/SceneGraph/CADEntity.h"

#include "VE_Xplorer/XplorerHandlers/WCS.h"

#include <osg/Geode>
#include <osg/Projection>

#include <osgText/Text>

//C/C++ libraries
#include <sstream>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
DisplayInformation::DisplayInformation()
{
   framerate_flag=false;
   coord_sys_flag=false;

   display_switch=new VE_SceneGraph::Switch;

   this->InitFrameRateDisplay();
   this->InitCoordSysDisplay();

   display_switch->AddChild(framerate.get());
   display_switch->AddChild(coord_sys.get());
}
////////////////////////////////////////////////////////////////////////////////
DisplayInformation::~DisplayInformation()
{
	;
}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::InitFrameRateDisplay()
{
   framerate=new VE_SceneGraph::DCS;
   /*
   framerate_dcs=new VE_SceneGraph::DCS;
   framerate_geode=new osg::Geode;
   framerate_text=new osgText::Text;
   framerate_font=osgText::readFontFile("../fonts/arial.ttf");
   
   framerate_text->setFont(framerate_font.get());
   framerate_text->setColor(osg::Vec4f(1.0f,1.0f,1.0f,1.0f));
   framerate_text->setCharacterSize(10.0f);
   //framerate_text->setRotation(osg::Quat(90.0f,osg::X_AXIS));
   framerate_text->setAlignment(osgText::Text::RIGHT_BASE_LINE);
   framerate_text->setFontResolution(40,40);

   framerate_geode->addDrawable(framerate_text.get());

   int windowWidth=VE_Xplorer::cfdEnvironmentHandler::instance()->GetWindowWidth();
   int windowHeight=VE_Xplorer::cfdEnvironmentHandler::instance()->GetWindowHeight();

   float position[3];
   position[0]=0;
   position[1]=10;
   position[2]=20;

   //framerate_text->setPosition(osg::Vec3(0.0f,10.0f,0.0f));
   framerate_dcs->SetTranslationArray(position);

   dynamic_cast<osg::Group*>(framerate_dcs->GetRawNode())->addChild(framerate_geode.get());
   VE_SceneGraph::cfdPfSceneManagement::instance()->GetRootNode()->AddChild(framerate_dcs);
   */
}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::InitCoordSysDisplay()
{
   coord_sys=new VE_SceneGraph::DCS;

   //The physical model for the world coordinate system display
   VE_SceneGraph::CADEntity* coord_sys_model=new VE_SceneGraph::CADEntity(GetVESuite_WCS(),coord_sys.get(),true);

   osg::ref_ptr<osg::Projection> projection=new osg::Projection;
   projection->setMatrix(osg::Matrix::ortho(-1.2,10,-1.2,10,-2,2));
   projection->setCullingActive(false);

   //wcs_stateset=new osg::StateSet(;)
   //wcs_display->GetRawNode()->setStateSet(wcs_stateset.get());

   //Disable depth testing so wcs is drawn regardless of depth values of geometry already drawn
   //wcs_stateset->setMode(GL_DEPTH_TEST,osg::StateAttribute::OFF);
}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::LatePreFrameUpdate()
{
   /*
   if(this->GetDisplayFrameRate()==true){
      std::stringstream ss(std::stringstream::in|std::stringstream::out);
      //ss<<framerate;
      ss<<" fps";
      framerate_text->setText(ss.str());
   }


   */
}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::FrameRateEvent()
{

}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::CoordSysEvent()
{

}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::SetFrameRateFlag(bool val)
{
   framerate_flag=val;
}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::SetCoordSysFlag(bool val)
{
   coord_sys_flag=val;
}
////////////////////////////////////////////////////////////////////////////////