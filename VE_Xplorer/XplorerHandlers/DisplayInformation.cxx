#include "VE_Xplorer/XplorerHandlers/DisplayInformation.h"

#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
#include "VE_Xplorer/SceneGraph/cfdDCS.h"
#include "VE_Xplorer/SceneGraph/cfdFILE.h"
#include "VE_Xplorer/SceneGraph/cfdSwitch.h"

#include "VE_Xplorer/XplorerHandlers/WCS.h"

#include <osg/Geode>

#include <osgText/Text>

//C/C++ libraries
#include <sstream>

using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////////
DisplayInformation::DisplayInformation()
{
   framerate_flag=false;
   coord_sys_flag=false;
}
////////////////////////////////////////////////////////////////////////////////
DisplayInformation::~DisplayInformation()
{
   if(display_switch){
      delete display_switch;
   }

   if(framerate){
      delete framerate;
   }

   if(coord_sys){
      delete coord_sys;
   }

}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::InitializeDisplay()
{
   display_switch=new VE_SceneGraph::cfdSwitch;
   framerate=new VE_SceneGraph::cfdDCS;
   coord_sys=new VE_SceneGraph::cfdFILE(GetVESuite_WCS(),VE_SceneGraph::cfdPfSceneManagement::instance()->GetRootNode(),true);

   display_switch->AddChild(framerate);

   this->InitFrameRateDisplay();
   this->InitCoordSysDisplay();
}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::InitFrameRateDisplay()
{
   /*
   framerate_dcs=new VE_SceneGraph::cfdDCS;
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
   //wcs_stateset=new osg::StateSet(;)
   //wcs_display->GetRawNode()->setStateSet(wcs_stateset.get());

   //Disable depth testing so wcs is drawn regardless of depth values of geometry already drawn
   //wcs_stateset->setMode(GL_DEPTH_TEST,osg::StateAttribute::OFF);

   float translation[3]={0,100,0};
   float scale[3]={1.0f,1.0f,1.0f};
   //coord_sys->SetTranslationArray(translation);
   //coord_sys->SetScaleArray(scale);
}
////////////////////////////////////////////////////////////////////////////////
void DisplayInformation::LatePreFrameUpdate()
{
   //std::cout<<this->GetDisplayCoordSys()<<std::endl;
   //std::cout<<this->GetDisplayFrameRate()<<std::endl<<std::endl;

   /*
   if(this->GetDisplayFrameRate()==true){
      std::stringstream ss(std::stringstream::in|std::stringstream::out);
      //ss<<framerate;
      ss<<" fps";
      framerate_text->setText(ss.str());
   }
   else{
      framerate_text->setText("");
   }

   if(this->GetDisplayCoordSys()==true){
      float wcs_translation[3];

      wcs_translation[0]=0.0f;
      wcs_translation[1]=0.0f;
      wcs_translation[2]=0.0f;

      wcs_display->SetTranslationArray(wcs_translation);
      //Draw wcs last
      wcs_stateset->setRenderBinDetails(1,"RenderBin");
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