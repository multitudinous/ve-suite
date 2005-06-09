#include "UI_Tabs.h"
#include "cfdEnum.h"
#include "UI_TransTab.h"
#include "UI_VertTab.h"
#include "UI_ModelData.h"
#include "UI_NavTab.h"
#include "UI_VisTab.h"
#include "UI_VecTab.h"
#include "UI_StreamTab.h"
#include "UI_TeacherTab.h"
#include "UI_SoundsTab.h"
#include "UI_GeometryTab.h"
#include "UI_ViewLocTab.h"
#include "UI_DesignParTab.h"
#ifdef VE_PATENTED
#ifdef CFD_USE_SHADERS
#include "UI_AdvectionPanel.h"
#endif
#endif

#include "controlIds.h"
#include <iostream>

////////////////////////////////////////////////////
//Constructor                                     //
////////////////////////////////////////////////////
UI_Tabs::UI_Tabs(VjObs_ptr ref, wxWindow* parent, UI_ModelData* _model, 
            int activeMod, 
            wxWindowID id,
            const wxPoint& pos ,
            const wxSize& size ,
            long style )
/*UI_Tabs::UI_Tabs(VjObs_ptr ref,wxWindow* parent, wxWindowID id,
            const wxPoint& pos ,
            const wxSize& size ,
            long style )*/
:wxNotebook(parent, id, pos, size, style)
{
   server_ref = VjObs::_duplicate(ref);

   //copied code from old Tabs.cpp
   numOfClientInfo = 9;
   clientInfoArray = new VjObs::obj_pd(50);
   clientInfoArray->length( numOfClientInfo );
   //clientInfoArray->length( 50 );

   _modelData = _model;
   _activeModIndex = activeMod;
   debugIO = false;

   getData();

}

void UI_Tabs::getData()
{
   cGeo_state = 0;
   cIso_value = 0;
   cTimesteps = 0;
   cMin = 0;
   cMax = 0;
   cSc = 0;

   _visPage = 0;
   _vectorPage = 0;
   _streamlinePage = 0;
   _navPage = 0;
   //cout<<"Act Mod Index3: "<<_activeModIndex<<endl;
   num_geo = _modelData->GetNumberOfGeomFiles(_activeModIndex);
   std::cout << "geo number: " << num_geo << std::endl;

   geoNameArray = VjObs::scalar_p( *_modelData->GetGeomFilenames(_activeModIndex) );
   geomFileSettings = _modelData->GetGeometryFileSettings( _activeModIndex );

   // Get Number of Sound Files
   if ( !CORBA::is_nil( server_ref ) )
   {
      num_sounds = server_ref->GetNumberOfSounds();
   }
   else
   {
      num_sounds = 0;
   }

   std::cout << "number of sound files: " << num_sounds << std::endl;
   if( num_sounds > 0 ){
      soundNameArray = server_ref->GetSoundNameArray();
   }

   // Get Number of Teacher Files
   if ( !CORBA::is_nil( server_ref ) )
   {
      num_teacher = server_ref->get_teacher_num();
   }
   else
   {
      num_teacher = 0;
   }
   
   std::cout << "teacher number: "
               << num_teacher << std::endl;
   if( num_teacher > 0 ) 
   {
      teacher_attrib = server_ref->get_teacher_name();
   }
   cTeacher_state = 0;

   cPre_state = 0;

   if ( !CORBA::is_nil( server_ref ) )
   {
      num_viewlocs = server_ref->getIsoValue();
   }
   std::cout << "number of viewing locations: "<< num_viewlocs << std::endl;
  /* server_ref = VjObs::_duplicate(ref);

   cGeo_state = 0;
   cIso_value = 0;
   cMin = 0;
   cMax = 0;

   //copied code from old Tabs.cpp
   numOfClientInfo = 9;
   clientInfoArray = new VjObs::obj_pd(50);
   clientInfoArray->length( numOfClientInfo );

   // Get Number of Geometry Files
   if ( !CORBA::is_nil( server_ref ) )
   {
      num_geo = server_ref->get_geo_num();
   }
   else
   {
      num_geo = 0;
   }
   std::cout << "geo number: " << num_geo << std::endl;
   if ( num_geo > 0 )
   {  
      if ( !CORBA::is_nil( server_ref ) )
      {
         geoNameArray = server_ref->get_geo_name();
      }
      else
      {
         geoNameArray = NULL;
      }
      std::cout << "geo number: " << num_geo << std::endl;
      for(CORBA::ULong i = 0; i < (unsigned int)num_geo; i++)
      std::cout << geoNameArray[ i ] << std::endl;
   }

   // Get Number of Sound Files
   if ( !CORBA::is_nil( server_ref ) )
   {
      numSounds = server_ref->GetNumberOfSounds();
   }
   else
   {
      numSounds = 0;
   }

   std::cout << "number of sound files: " << numSounds << std::endl;
   if( numSounds > 0 ){
      soundNameArray = server_ref->GetSoundNameArray();
   }

   // Get Number of Teacher Files
   if ( !CORBA::is_nil( server_ref ) )
   {
      num_teacher = server_ref->get_teacher_num();
   }
   else
   {
      num_teacher = 0;
   }
   
   std::cout << "teacher number: "
               << num_teacher << std::endl;
   if( num_teacher > 0 ) {
      teacher_attrib = server_ref->get_teacher_name();
   }
   cTeacher_state = 0;

   if ( !CORBA::is_nil( server_ref ) )
   {
      datasetNum = server_ref->get_sc_num();
   }
   else
   {
      datasetNum = 0;
   }
   
   std::cout << "number of datasets: "
               << datasetNum << std::endl;

   int numScalarsInFirstDataset = 0;
   if( datasetNum > 0 ) {
      CORBA::ULong index;
      index = 0;
      sc_attrib = server_ref->update_scalar();  //all scalar names
      std::cout << "first scalar: "
                  << sc_attrib[ index ] << std::endl;

      //allocate space for the arrays that store scalar min and max (0-100)
      sc_min = new int [66];//TODO fix hardcoded limits
      sc_max = new int [66];

      // initialize to full scale...
      for( int i = 0; i < 66; i++) {
         sc_min[i] = 0;
         sc_max[i] = 100;
      }

      datasetNames = server_ref->get_dataset_names();
      std::cout << "first dataset: "
                  << datasetNames[ index ] << std::endl;

      datasetTypes = server_ref->get_dataset_types();
      std::cout << "first dataset type: "
                  << datasetTypes[0] << std::endl;

      numScalarsPerDataset = server_ref->get_num_scalars_per_dataset();
      numScalarsInFirstDataset = numScalarsPerDataset[0];
      std::cout << "numScalars in first dataset: "
                  << numScalarsInFirstDataset << std::endl;
   }

   numScalarsInActiveDataset = numScalarsInFirstDataset;
   
   short postDataFlag;
   if ( !CORBA::is_nil( server_ref ) )
   {
      postDataFlag = server_ref->getPostdataState();
   }
   else
   {
      postDataFlag = 0;
   }
   
   std::cout << "postdata number: " << postDataFlag << std::endl;

   hasXPostData = hasYPostData = hasZPostData = false;
   if ( postDataFlag == 1 || postDataFlag == 3 ||
        postDataFlag == 5 || postDataFlag == 7 )
   {
      hasXPostData = true;
   }
   if ( postDataFlag == 2 || postDataFlag == 3 ||
        postDataFlag == 6 || postDataFlag == 7 )
   {
      hasYPostData = true;
   }
   if ( postDataFlag == 4 || postDataFlag == 5 ||
        postDataFlag == 6 || postDataFlag == 7 )
   {
      hasZPostData = true;
   }

   if ( !CORBA::is_nil( server_ref ) )
   {
      cTimesteps = server_ref->getTimesteps();
   }
   else
   {
      cTimesteps = 0;
   }
   
   std::cout << "VE_Conductor : timesteps : " << cTimesteps << std::endl;

   cPre_state = 0;*/

}

UI_Tabs::~UI_Tabs()
{
	//delete clientInfoArray;
	/* delete sc_min;
    delete sc_max;
	 delete _visPage;
    delete _geometryPage;
    delete _soundPage;
    delete _streamlinePage;
    delete _teacherPage;
    delete _vectorPage;
    delete _navPage;
    delete _transPage;
    delete _vertPage;
    delete _designparPage;*/
}
/////////////////////////////////////////////////////
//update the active scalar on the                  //
//dataset page                                     //
/////////////////////////////////////////////////////
/*void UI_Tabs::changeActiveScalarOnDataset(const char* name)
{
   if(name){
      if(_datasetPage){
         _datasetPage->makeActiveScalarOnDataset(name);
      } 
   }
}*/
////////////////////////////////////////////////
//specify the active dataset                  //
////////////////////////////////////////////////
void UI_Tabs::setActiveDataset(int whichDataset)
{
   cId = CHANGE_STEADYSTATE_DATASET;
   cIso_value = whichDataset;
   sendDataArrayToServer();

}
//////////////////////////////////////////////
/*void UI_Tabs::setActiveScalar(int whichScalar)
{
   if(_scalarPage){
      _scalarPage->setActiveScalar(whichScalar);
   }
}
////////////////////////////////
void UI_Tabs::_initDatasetPage(UI_DatasetTab* _datasetPage)
{
   //set the number of data sets
   if(!_datasetPage){
      cout<<"Data set page not created yet!!!"<<endl;
      return;
   }
   //initialize the number of datasets
   _datasetPage->setNumberOfDatasets(datasetNum);

   int nPoly = 0;
   int nVert = 0;
   int n3Dmesh = 0;

   //collect all the data for each 
   //data set
   CORBA::ULong index =0;
   for(CORBA::ULong i = 0; i < datasetNum; i++){
      //a class describing each dataset
      UI_DatasetInfo* dataSet = new UI_DatasetInfo();

      //the type
      dataSet->setDatasetType(datasetTypes[i]);

      //keep track of the number of types
      //we've found
      switch (datasetTypes[i]){
         case 0:
            nPoly++;
            break;
         case 1:
            nVert++;
            break;
         case 2:
            n3Dmesh++;
            break;
      };
      

      //the name
      char* name = new char[strlen(datasetNames[i])+1];
      strcpy(name,datasetNames[i]);
      dataSet->setDatasetName(name);

      if(name){
         delete [] name;
         name = 0;
      }
      //gotta be something better than this
      int numScalars = numScalarsPerDataset[i];

      char** scalarNames = new char*[numScalars];

      //the scalar names 
      for(int f = 0; f < numScalars; f++){
        scalarNames[f] = new char[strlen(sc_attrib[index])+1];
        strcpy( scalarNames[f],  sc_attrib[index++]);
      }
      //set the scalar data for this 
      dataSet->setScalarData(scalarNames,numScalars);

      //add the data set
      _datasetPage->addDataset(dataSet);

     //clean up the names array
      for(int p = numScalars -1; p >= 0; p--){
         delete [] scalarNames[p];   
      }
      delete []  scalarNames;
   }
   //process the found types

   //probably a better way to do this
   int* nDataTypes =  new int[3];

   nDataTypes[0] = nPoly;
   nDataTypes[1] = nVert;
   nDataTypes[2] = n3Dmesh;

   //pass it on to the page
   _datasetPage->setFoundDataTypes(nDataTypes);

   //update the page
   _datasetPage->updateView();


   if(nDataTypes){
      delete [] nDataTypes;
      nDataTypes = 0;
   }

}
//////////////////////////////////////////////////////////////////////////
void UI_Tabs::updateScalarPage(wxString* names, int numNames, int refresh)
{
   if(_scalarPage){
      _scalarPage->updateScalarTabRadioBoxInfo(numNames,names);
   }
}
///////////////////////////////////////////////////////////////////////
void UI_Tabs::updateScalarPage(char** names, int numNames, int refresh)
{
   if(_scalarPage){
      _scalarPage->updateScalarTabRadioBoxInfo(numNames,names);
   }
}*/
//////////////////////////////
//Create the pages of the UI//
//////////////////////////////
void UI_Tabs::createTabPages()
{
   //Add the pages to the notebook
 
   //Visualization page
   _visPage = new UI_VisualizationTab(this);  
   AddPage( _visPage, _T("Visualization"), true); 
   
   //DataSets page
   //_datasetPage = new UI_DatasetTab(this);
   //AddPage( _datasetPage, _T("DataSets"), false );

   //set up the dataset tab
   //_initDatasetPage();
   
   //Geometry page
   _geometryPage = new UI_GeometryTab(this);
   AddPage( _geometryPage, _T("Geometry"), false );  


   //Scalars page
   /*if(numScalarsInActiveDataset){
      //get the names from the server to put in the radio box
      char** scalarNames = new char*[numScalarsInActiveDataset];
      for(CORBA::ULong i = 0; i< numScalarsInActiveDataset; i++){
         scalarNames[i] = new char[strlen(sc_attrib[i])+1];
         strcpy(scalarNames[i],sc_attrib[i]);
      }

      //the new page w/ the appropriate names in the radio box
      //_scalarPage = new UI_ScalarTab(this,numScalarsInActiveDataset,scalarNames);

      //clean up
      for(int j = numScalarsInActiveDataset - 1; j >= 0; j--){
         delete [] scalarNames[j];
         scalarNames[j] = 0;
      }
      delete [] scalarNames;
   }else{
      //a default string w/ no scalar names 
      wxString noScalarString[] = {wxT("No Scalars")};
       //_scalarPage = new UI_ScalarTab(this,1,(char**) noScalarString);
   }
   //AddPage( _scalarPage, _T("Scalars"), false );*/

   //Sounds page
   _soundPage = new UI_SoundTab(this);
   AddPage( _soundPage, _T("Sounds"), false );

   //Streamlines page
   _streamlinePage = new UI_StreamlineTab(this);
   AddPage( _streamlinePage, _T("Streamlines"), false );

   //Teacher page
   _teacherPage = new UI_TeacherTab(this);
   AddPage( _teacherPage, _T("Teacher"), false );

   //Vectors page
   _vectorPage = new UI_VectorTab(this);
   AddPage( _vectorPage, _T("Vectors/Contours"), false );

   //Navigation page
   _navPage = new UI_NavigationTab(this);
   AddPage( _navPage, _T("Navigation"), false );
  
   //Navigation page
   // This tab needs to be moved to the UI_VisTab
   //_transPage = new UI_TransTab(this);
   //AddPage( _transPage, _T("Transient"), false );

   //Navigation page
   _vertPage = new UI_VertTab(this);
   AddPage( _vertPage, _T("Vertex"), false );

   //Viewing Locations page
   _viewlocPage = new UI_ViewLocTab(this);
   AddPage( _viewlocPage, _T("View Points"), false );
#ifdef VE_PATENTED
#ifdef CFD_USE_SHADERS
   _advectionPage = new UI_AdvectionPanel(this);
   AddPage(_advectionPage,_T("Texture-Based Tools"));
#endif
#endif
   //SetSelection(0);
}

#ifdef VE_PATENTED
#ifdef CFD_USE_SHADERS
////////////////////////////////////
void UI_Tabs::EnableAdvectionPage()
{
   if(_advectionPage)
   {
      _advectionPage->Enable(true);
   }
}
////////////////////////////////////
void UI_Tabs::DisableAdvectionPage()
{
   if(_advectionPage)
   {
      _advectionPage->Enable(false);
   }
}
#endif
#endif
///////////////////////////////////////////
void UI_Tabs::rebuildTabPages( int activeMod )
{
   _activeModIndex = activeMod;
   getData();
   createTabPages();
}

///////////////////////////////////////////
void UI_Tabs::sendDataArrayToServer( void )
{
   clientInfoArray[ 0 ] = (double)cId;
   clientInfoArray[ 1 ] = (double)cIso_value;
   clientInfoArray[ 2 ] = (double)cTimesteps;
   clientInfoArray[ 3 ] = (double)cSc;
   clientInfoArray[ 4 ] = (double)cMin;
   clientInfoArray[ 5 ] = (double)cMax;
   clientInfoArray[ 6 ] = (double)cGeo_state;
   clientInfoArray[ 7 ] = (double)cPre_state;
   clientInfoArray[ 8 ] = (double)cTeacher_state;
   
   if ( debugIO )
   {
      std::cout << " Construct data array to send to server side : " << std::endl;
      std::cout << "    command id     : " << clientInfoArray[ 0 ] << std::endl;
      std::cout << "    iso_value      : " << clientInfoArray[ 1 ] << std::endl;
      std::cout << "    timesteps      : " << clientInfoArray[ 2 ] << std::endl;
      std::cout << "    sc             : " << clientInfoArray[ 3 ] << std::endl;
      std::cout << "    min            : " << clientInfoArray[ 4 ] << std::endl;
      std::cout << "    max            : " << clientInfoArray[ 5 ] << std::endl;
      std::cout << "    geo_state      : " << clientInfoArray[ 6 ] << std::endl;
      std::cout << "    pre_state      : " << clientInfoArray[ 7 ] << std::endl;
      std::cout << "    teacher_state  : " << clientInfoArray[ 8 ] << std::endl;
   }

   if ( !CORBA::is_nil( server_ref ) )
   {
      std::cout << " Setting client data " << std::endl;
      server_ref->SetClientInfoData( clientInfoArray );
      std::cout << " Done Setting client data " << std::endl;
   }
   else
   {
      std::cout << "VE_Conductor : Just testing..." << std::endl;
   }
}
