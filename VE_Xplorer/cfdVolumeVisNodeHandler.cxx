#ifdef _OSG

#include <osg/Geometry>
#include <osg/Geode>
#include <osg/Group>
#include <osg/Switch>
#include <iostream>
#include "cfdVolumeVisNodeHandler.h"
#include "cfdTextureManager.h"
//////////////////////////////////////////////////
//Constructors                                  //
//////////////////////////////////////////////////
cfdVolumeVisNodeHandler::cfdVolumeVisNodeHandler()
{
   _whichChildIsThis = 0;
   _tm = 0;
}
//////////////////////////////////////////////////////
cfdVolumeVisNodeHandler::cfdVolumeVisNodeHandler(const
                          cfdVolumeVisNodeHandler& vvnh)
{
   _bbox = vvnh._bbox;
   _bboxSwitch = new osg::Switch(*vvnh._bboxSwitch);
   _byPassNode = new osg::Group(*vvnh._byPassNode);
   _vvN = new osg::Switch(*vvnh._vvN);
   _whichChildIsThis = vvnh._whichChildIsThis;
   _decoratorGroup = new osg::Group(*vvnh._decoratorGroup);
   _tm = new cfdTextureManager(*vvnh._tm);
}
///////////////////////////////////////////////////
//Destructor                                     //
///////////////////////////////////////////////////
cfdVolumeVisNodeHandler::~cfdVolumeVisNodeHandler()
{
   if(_tm){
      delete _tm;
      _tm = 0;
   }
}
////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetBoundingBox(float* bbox)
{
   float minBBox[3];
   float maxBBox[3];
   //this is because vtk gives mnx,mxx,mny,mxy,mnz,mxz
   minBBox[0] = bbox[0]; 
   minBBox[1] = bbox[2]; 
   minBBox[2] = bbox[4]; 
   maxBBox[0] = bbox[1]; 
   maxBBox[1] = bbox[3]; 
   maxBBox[2] = bbox[5]; 
   _bbox.set(osg::Vec3(minBBox[0],minBBox[1],minBBox[2]), 
                osg::Vec3(maxBBox[0],maxBBox[1],maxBBox[2]));
  
}
///////////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetSwitchNode(osg::Switch* vvn)
{
   _vvN = vvn;
}
////////////////////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetTextureManager(cfdTextureManager* tm)
{
   _tm = tm;
}
////////////////////////////////////
void cfdVolumeVisNodeHandler::Init()
{
   if(!_bbox.valid()){
      std::cout<<"Invalid bounding box!!"<<std::endl;
      std::cout<<"cfdVolumeVizNodeHandler::Init!!"<<std::endl;
      return;
   }
   if(!_vvN.valid()){
      std::cout<<"Invalid volume viz node!!"<<std::endl;
      std::cout<<"cfdVolumeVizNodeHandler::Init!!"<<std::endl;
      return;
   }
   if(!_tm){
      std::cout<<"Invalid TextureManager!!"<<std::endl;
      std::cout<<"cfdVolumeVisNodeHandler::Init!!"<<std::endl;
      return;
   }
   
   if(!_bboxSwitch.valid()){
      _createVisualBBox();
      _whichChildIsThis = _vvN->getNumChildren();
      //be able to turn the bounding box off/on
      _bboxSwitch = new osg::Switch();
      _bboxSwitch->setName("VVNH BBox Switch");
      _bboxSwitch->addChild(_visualBoundingBox.get());
      _bboxSwitch->setSingleChildOn(0);
      _vvN->addChild(_bboxSwitch.get());

      //set up the decorator nodes
      if(!_decoratorGroup.valid()){
         _decoratorGroup = new osg::Group();
      }

      _visualBoundingBox->addChild(_decoratorGroup.get());
      _bboxSwitch->addChild(_decoratorGroup.get());
      
      //need to bypass the "decorator" level to attach
      _byPassNode = dynamic_cast<osg::Group*>
	                  (((osg::Group*)_vvN->getChild(0))->getChild(0));
      
      //hook up the decorator
      _decoratorGroup->addChild(_byPassNode.get());

      //NOTE -- In derived classes, must override this call
      //to setup the stateset for the decorator
      _setUpDecorator();

      //must do this to make sure switch is initially
      //traversing the "undecorated" node
      _vvN->setSingleChildOn(0);
   }
}
////////////////////////////////////////////
bool cfdVolumeVisNodeHandler::IsThisActive()
{
   if(_vvN.valid()){
      int index = _whichChildIsThis - 1;
      return _vvN->getValue(index);
   }
   return false;
}
////////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetBoundingBoxName(char* name)
{
   if(name && _bboxSwitch.valid()){
      _bboxSwitch->setName(name);
   }
}
////////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetDecoratorName(char* name)
{
   if(name && _decoratorGroup.valid()){
      _decoratorGroup->setName(name);
   }
}
///////////////////////////////////////////////
void cfdVolumeVisNodeHandler::EnableDecorator()
{
   if(_vvN.valid()){
      _vvN->setSingleChildOn(_whichChildIsThis);
   }
}
//////////////////////////////////////////
void cfdVolumeVisNodeHandler::TurnOnBBox()
{
   if(_bboxSwitch.valid()){
      _bboxSwitch->setSingleChildOn(0);
   } 
}
///////////////////////////////////////////
void cfdVolumeVisNodeHandler::TurnOffBBox()
{
   if(_bboxSwitch.valid()){
      _bboxSwitch->setSingleChildOn(1);
   } 
}
/////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::_createVisualBBox()
{
   if(_bbox.valid()){
      _visualBoundingBox = new osg::Group();
      _visualBoundingBox->setName("VisHandler Bounding Box");

      osg::ref_ptr<osg::StateSet> ss = _visualBoundingBox->getOrCreateStateSet();
      ss->setRenderingHint(osg::StateSet::OPAQUE_BIN);
      ss->setMode(GL_LIGHTING,osg::StateAttribute::OFF);
      
      osg::Geometry* bboxCube = new osg::Geometry;

      osg::Vec3Array* coords = new osg::Vec3Array();
      coords->push_back(_bbox.corner(0));
      coords->push_back(_bbox.corner(1));
      coords->push_back(_bbox.corner(2));
      coords->push_back(_bbox.corner(3));
      coords->push_back(_bbox.corner(4));
      coords->push_back(_bbox.corner(5));
      coords->push_back(_bbox.corner(6));
      coords->push_back(_bbox.corner(7));

      coords->push_back(_bbox.corner(0));
      coords->push_back(_bbox.corner(2));
      coords->push_back(_bbox.corner(4));
      coords->push_back(_bbox.corner(6));
      coords->push_back(_bbox.corner(1));
      coords->push_back(_bbox.corner(3));
      coords->push_back(_bbox.corner(5));
      coords->push_back(_bbox.corner(7));

      coords->push_back(_bbox.corner(0));
      coords->push_back(_bbox.corner(4));
      coords->push_back(_bbox.corner(1));
      coords->push_back(_bbox.corner(5));
      coords->push_back(_bbox.corner(2));
      coords->push_back(_bbox.corner(6));
      coords->push_back(_bbox.corner(3));
      coords->push_back(_bbox.corner(7));

      bboxCube->setVertexArray(coords);

      osg::Vec4Array* colors = new osg::Vec4Array(1);
      (*colors)[0].set(1.0f,1.0f,0.0f,1.0f);
      bboxCube->setColorArray(colors);
      bboxCube->setColorBinding(osg::Geometry::BIND_OVERALL);
      bboxCube->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES,
			       0,coords->size()));
      
      osg::Geode* geode = new osg::Geode;
      geode->addDrawable(bboxCube);
      geode->setName("Visual BBox");
      _visualBoundingBox->addChild(geode);
      
   }else{
      std::cout<<"Invalid bbox in cfdVolumeVisNodeHandler::_createVisualBBox!"<<std::endl;
   }
}

///////////////////////////////////////////////////////////////////////
//equal operator                                                     //
///////////////////////////////////////////////////////////////////////
cfdVolumeVisNodeHandler&
cfdVolumeVisNodeHandler::operator=(const cfdVolumeVisNodeHandler& vvnh)
{
   if(this != &vvnh){
      _bbox = vvnh._bbox;
      _vvN = vvnh._vvN;
      _decoratorGroup = vvnh._decoratorGroup;
      _visualBoundingBox = vvnh._visualBoundingBox;
      _bboxSwitch = vvnh._bboxSwitch;
      _tm = vvnh._tm;
      _byPassNode = vvnh._byPassNode;
   }
   return *this;
}

#endif //_OSG
