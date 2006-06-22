#include <osg/Node>
#include <osg/Geometry>
#include <osg/Notify>
#include <osg/Texture3D>
#include <osg/TexGen>
#include <osg/Geode>
#include <osg/Billboard>
#include <osg/PositionAttitudeTransform>
#include <osg/ClipNode>
#include <osg/AlphaFunc>
#include <osg/TexGenNode>
#include <osg/TexEnvCombine>
#include <osg/Material>
/*
#include <osgNVCg/Context>
#include <osgNVCg/Program>
#include <osgNVCg/CgGeometry>
#include <osgNV/StateMatrixParameterValue>
#include <osgNV/MatrixParameterValue>
#include <osgNV/VectorParameterValue>

#include <osgNV/LightPicker>
#include <osgNV/ViewMatrixPicker>
#include <osgNV/ModelMatrixPicker>
#include <osgNV/Version>
*/
#include <osgDB/Registry>
#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgDB/FileNameUtils>

#include <osgUtil/CullVisitor>
#include <osg/Switch>
#include <Producer/Camera>
#include <osgProducer/Viewer>
#include <fstream>
#include <iostream>

#include "cfdTextureManager.h"

#include "cfdVolumeVisualization.h"
#include "cfdVectorVolumeVisHandler.h"
#include "cfdScalarVolumeVisHandler.h"
#include "cfdPBufferManager.h"
#include "cfdInitializePbufferCallback.h"
#include "cfdOSGGammaShaderManager.h"

class cfdPostSwapCallback:public Producer::Camera::Callback
{
public:
   cfdPostSwapCallback(cfdVectorVolumeVisHandler* vh)
   {
      _vvh = vh;
   };
   virtual ~cfdPostSwapCallback(){};
   virtual void operator()(const Producer::Camera &)
   {
      std::cout<<"Swapping"<<std::endl;
      if(_vvh){
         _vvh->PingPongTextures();
      }
      std::cout<<"========swapped======="<<std::endl;
   }
protected:
   cfdVectorVolumeVisHandler* _vvh;
};
int main( int argc, char **argv )
{
   char name[256];
   cfdPBufferManager* pbm= new cfdPBufferManager();

   // use an ArgumentParser object to manage the program arguments.
   osg::ArgumentParser arguments(&argc,argv);
    
   cfdTextureManager* tm = new cfdTextureManager();
   if(arguments.read("-dataFile")) 
   {
      for(int pos=1;pos<arguments.argc() && !arguments.isOption(pos);++pos)
      {
         std::ifstream fin(arguments[pos]);
         if(fin.is_open()){ 
            int numFiles = 0;
            fin>>numFiles;
            std::string tempName;
            std::getline( fin, tempName );
            std::getline( fin, tempName );
            for(int i = 0; i < numFiles; i++){
               fin>>name;
               tm->SetUseShaders(true);
               tm->addFieldTextureFromFile(name);
            }
         }
      }
   }else{
      std::cout<<"Error! No input file specified!!"<<std::endl;
      exit(1);
   }

    // construct the viewer.
   osgProducer::Viewer viewer(arguments);

   // set up the value with sensible default event handlers.
   viewer.setUpViewer(osgProducer::Viewer::STANDARD_SETTINGS);

   // get details on keyboard and mouse bindings used by the viewer.
   viewer.getUsage(*arguments.getApplicationUsage());

   // if user request help write it out to cout.
   if(arguments.read("-h") || arguments.read("--help")){
      arguments.getApplicationUsage()->write(std::cout);
      return 1;
   }

   // any option left unread are converted into errors to write out later.
   arguments.reportRemainingOptionsAsUnrecognized();

   // report any errors if they have occured when parsing the program aguments.
   if(arguments.errors()){
      arguments.writeErrorMessages(std::cout);
      return 1;
   }
   

   // create a model from the images.
   cfdVolumeVisualization* vvNode = new cfdVolumeVisualization();
   vvNode->SetVeboseFlag(true);
   vvNode->SetNumberofSlices(100);
   vvNode->SetSliceAlpha(.2);
   vvNode->SetTextureManager(tm);
   //vvNode->CreateNode();
   
   osg::ref_ptr<osg::Group> rootNode = vvNode->GetVolumeVisNode().get();
 
   /*cfdVectorVolumeVisHandler* vvvh = new cfdVectorVolumeVisHandler();
   vvvh->SetBoundingBox(tm->getBoundingBox());
   vvvh->SetSwitchNode(vvNode->GetVolumeVisNode().get());
   vvvh->SetTextureScale(vvNode->GetTextureScale(),false);
   vvvh->SetCenter(vvNode->GetBBoxCenter());

   vvvh->SetAttachNode(vvNode->GetDecoratorAttachNode().get());
   vvvh->SetTextureManager(tm);*/
   
   cfdScalarVolumeVisHandler* svvh = new cfdScalarVolumeVisHandler();
   svvh->SetBoundingBox(tm->getBoundingBox());
   
   svvh->SetSwitchNode(vvNode->GetVolumeVisNode().get());
   svvh->SetTextureScale(vvNode->GetTextureScale(),false);
   svvh->SetCenter(vvNode->GetBBoxCenter());

         
   svvh->SetAttachNode(vvNode->GetDecoratorAttachNode().get());
   svvh->SetTextureManager(tm);
   float* currentBBox = tm->getBoundingBox();
   //create an x plane
   double xplane[4] = {1,0,0,0};
   float alpha = .35;//(float)_cmdArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
   xplane[3] = currentBBox[0] + alpha*(currentBBox[1] - currentBBox[0]);
   //xplane[3] = currentBBox[4] + alpha*(currentBBox[5] - currentBBox[4]);
   xplane[3] *=-1.0;
  vvNode->UpdateClipPlanePosition(cfdVolumeVisualization::XPLANE_MIN,xplane);
   //get the xplane positions
   alpha = .5;
   xplane[0] = 0;
   xplane[1] = 1;
    xplane[3] = currentBBox[2] + alpha*(currentBBox[3] - currentBBox[2]);
   xplane[3] *=-1.0;
   //vvNode->UpdateClipPlanePosition(cfdVolumeVisualization::YPLANE,xplane);
   //get the xplane positions
   xplane[0] = 0;
   xplane[1] = 0;
   xplane[2] = 1;
    xplane[3] = currentBBox[4] + alpha*(currentBBox[5] - currentBBox[4]);
   xplane[3] *=-1.0;
   //vvNode->UpdateClipPlanePosition(cfdVolumeVisualization::ZPLANE,xplane);

   cfdInitializePbufferCallback* pbCallback=0;
   
   if (rootNode.valid()){

        // set the scene to render
        viewer.setSceneData(rootNode.get());
        viewer.setClearColor(osg::Vec4(0,0,0,0));
        
        // the construct state uses gl commands to resize images so we are forced
        // to only call it once a valid graphics context has been established,
        // for that we use a realize callback.
        pbCallback = new cfdInitializePbufferCallback();
        viewer.setRealizeCallback(pbCallback);

        // create the windows and run the threads.
        viewer.realize();
        svvh->Init();
        svvh->GetGammaShaderManager()->UpdateScalarMax(1.0);
        //vvvh->SetPBufferManager(pbCallback->pbuffer());
        //vvvh->Init();
        //viewer.getCamera(0)->addPostSwapCallback(new cfdPostSwapCallback(vvvh));
        while( !viewer.done() )
        {
           //aVVNode->setPBufferManager(pbCallback->pbuffer());
          
           svvh->Init();
           svvh->EnableDecorator();
           //std::cout<<"enable decorator"<<std::endl;
          //vvvh->EnableDecorator();
            // wait for all cull and draw threads to complete.
          //std::cout<<"sync"<<std::endl;
            viewer.sync();
            // update the scene by traversing it with the the update visitor which will
            // call all node update callbacks and animations.
            //std::cout<<"update"<<std::endl;
            viewer.update();

            // fire off the cull and draw traversals of the scene.
            //std::cout<<"frame begin"<<std::endl;
            viewer.frame();
            //std::cout<<"frame end"<<std::endl;
        }
        
        // wait for all cull and draw threads to complete before exit.
        viewer.sync();
        
    }    
   osgDB::writeNodeFile(*rootNode.get(),"./scene.osg");

    
    return 0;
}
