//
// Example code for implementing IDL interfaces in file ./idl/VjObs.idl
//

#include <iostream.h>
#include <VjObs.h>

//
// Example class implementing IDL interface VjObs
//
class VjObs_i: public POA_VjObs,
                public PortableServer::RefCountServantBase {
private:
  // Make sure all instances are built on the heap by making the
  // destructor non-public
  //virtual ~VjObs_i();
public:
  // standard constructor
  VjObs_i();
  virtual ~VjObs_i();

  // methods corresponding to defined IDL attributes and operations
  void update();
  VjObs::scalar_p* update_scalar();
  VjObs::scalar_p* update_vector();
  VjObs::scalar_p* get_geo_name();
  VjObs::scalar_p* get_teacher_name();
  CORBA::Short get_sc_num();
  CORBA::Short get_geo_num();
  CORBA::Short get_teacher_num();
  char* get_perf();
  void SetClientInfoFlag(CORBA::Short value);
  void SetClientInfoData(const VjObs::obj_p& o);
  VjObs::obj_p* GetClientInfoData();
  CORBA::Short GetNumberOfSounds();
  VjObs::scalar_p* GetSoundNameArray();
  VjObs::scalar_p* get_dataset_names();
  VjObs::obj_p* get_dataset_types();
  VjObs::obj_p* get_num_scalars_per_dataset();
  VjObs::obj_p* get_num_vectors_per_dataset();
  void setNumDatasets(CORBA::Short value);
  CORBA::Short getNumDatasets();
  CORBA::Short getTotalNumberOfScalars();
  void setNumVectors(CORBA::Short value);
  CORBA::Short getNumVectors();
  void setNumGeoArrays(CORBA::Short value);
  CORBA::Short getNumGeoArrays();
  void setClients(CORBA::Long value);
  CORBA::Long getClients();
  void setIsoValue(CORBA::Long value);
  CORBA::Long getIsoValue();
  void setSc(CORBA::Long value);
  CORBA::Long getSc();
  void setMin(CORBA::Long value);
  CORBA::Long getMin();
  void setMax(CORBA::Long value);
  CORBA::Long getMax();
  void setId(CORBA::Long value);
  CORBA::Long getId();
  void setGeoState(CORBA::Long value);
  CORBA::Long getGeoState();
  void setPostdataState(CORBA::Short value);
  CORBA::Short getPostdataState();
  void setPreState(CORBA::Short value);
  CORBA::Short getPreState();
  void setTimesteps(CORBA::Short value);
  CORBA::Short getTimesteps();
  void setNumTeacherArrays(CORBA::Short value);
  CORBA::Short getNumTeacherArrays();
  void setTeacherState(CORBA::Short value);
  CORBA::Short getTeacherState();

};

//
// Example implementational code for IDL interface VjObs
//
VjObs_i::VjObs_i(){
  // add extra constructor code here
}
VjObs_i::~VjObs_i(){
  // add extra destructor code here
}
//   Methods corresponding to IDL attributes and operations
void VjObs_i::update(){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::update()>"
}

VjObs::scalar_p* VjObs_i::update_scalar(){
  // insert code here and remove the warning
  #warning "Code missing in function <VjObs::scalar_p* VjObs_i::update_scalar()>"
}

VjObs::scalar_p* VjObs_i::update_vector(){
  // insert code here and remove the warning
  #warning "Code missing in function <VjObs::scalar_p* VjObs_i::update_vector()>"
}

VjObs::scalar_p* VjObs_i::get_geo_name(){
  // insert code here and remove the warning
  #warning "Code missing in function <VjObs::scalar_p* VjObs_i::get_geo_name()>"
}

VjObs::scalar_p* VjObs_i::get_teacher_name(){
  // insert code here and remove the warning
  #warning "Code missing in function <VjObs::scalar_p* VjObs_i::get_teacher_name()>"
}

CORBA::Short VjObs_i::get_sc_num(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Short VjObs_i::get_sc_num()>"
}

CORBA::Short VjObs_i::get_geo_num(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Short VjObs_i::get_geo_num()>"
}

CORBA::Short VjObs_i::get_teacher_num(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Short VjObs_i::get_teacher_num()>"
}

char* VjObs_i::get_perf(){
  // insert code here and remove the warning
  #warning "Code missing in function <char* VjObs_i::get_perf()>"
}

void VjObs_i::SetClientInfoFlag(CORBA::Short value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::SetClientInfoFlag(CORBA::Short value)>"
}

void VjObs_i::SetClientInfoData(const VjObs::obj_p& o){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::SetClientInfoData(const VjObs::obj_p& o)>"
}

VjObs::obj_p* VjObs_i::GetClientInfoData(){
  // insert code here and remove the warning
  #warning "Code missing in function <VjObs::obj_p* VjObs_i::GetClientInfoData()>"
}

CORBA::Short VjObs_i::GetNumberOfSounds(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Short VjObs_i::GetNumberOfSounds()>"
}

VjObs::scalar_p* VjObs_i::GetSoundNameArray(){
  // insert code here and remove the warning
  #warning "Code missing in function <VjObs::scalar_p* VjObs_i::GetSoundNameArray()>"
}

VjObs::scalar_p* VjObs_i::get_dataset_names(){
  // insert code here and remove the warning
  #warning "Code missing in function <VjObs::scalar_p* VjObs_i::get_dataset_names()>"
}

VjObs::obj_p* VjObs_i::get_dataset_types(){
  // insert code here and remove the warning
  #warning "Code missing in function <VjObs::obj_p* VjObs_i::get_dataset_types()>"
}

VjObs::obj_p* VjObs_i::get_num_scalars_per_dataset(){
  // insert code here and remove the warning
  #warning "Code missing in function <VjObs::obj_p* VjObs_i::get_num_scalars_per_dataset()>"
}

VjObs::obj_p* VjObs_i::get_num_vectors_per_dataset(){
  // insert code here and remove the warning
  #warning "Code missing in function <VjObs::obj_p* VjObs_i::get_num_vectors_per_dataset()>"
}

void VjObs_i::setNumDatasets(CORBA::Short value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setNumDatasets(CORBA::Short value)>"
}

CORBA::Short VjObs_i::getNumDatasets(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Short VjObs_i::getNumDatasets()>"
}

CORBA::Short VjObs_i::getTotalNumberOfScalars(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Short VjObs_i::getTotalNumberOfScalars()>"
}

void VjObs_i::setNumVectors(CORBA::Short value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setNumVectors(CORBA::Short value)>"
}

CORBA::Short VjObs_i::getNumVectors(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Short VjObs_i::getNumVectors()>"
}

void VjObs_i::setNumGeoArrays(CORBA::Short value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setNumGeoArrays(CORBA::Short value)>"
}

CORBA::Short VjObs_i::getNumGeoArrays(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Short VjObs_i::getNumGeoArrays()>"
}

void VjObs_i::setClients(CORBA::Long value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setClients(CORBA::Long value)>"
}

CORBA::Long VjObs_i::getClients(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Long VjObs_i::getClients()>"
}

void VjObs_i::setIsoValue(CORBA::Long value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setIsoValue(CORBA::Long value)>"
}

CORBA::Long VjObs_i::getIsoValue(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Long VjObs_i::getIsoValue()>"
}

void VjObs_i::setSc(CORBA::Long value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setSc(CORBA::Long value)>"
}

CORBA::Long VjObs_i::getSc(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Long VjObs_i::getSc()>"
}

void VjObs_i::setMin(CORBA::Long value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setMin(CORBA::Long value)>"
}

CORBA::Long VjObs_i::getMin(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Long VjObs_i::getMin()>"
}

void VjObs_i::setMax(CORBA::Long value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setMax(CORBA::Long value)>"
}

CORBA::Long VjObs_i::getMax(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Long VjObs_i::getMax()>"
}

void VjObs_i::setId(CORBA::Long value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setId(CORBA::Long value)>"
}

CORBA::Long VjObs_i::getId(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Long VjObs_i::getId()>"
}

void VjObs_i::setGeoState(CORBA::Long value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setGeoState(CORBA::Long value)>"
}

CORBA::Long VjObs_i::getGeoState(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Long VjObs_i::getGeoState()>"
}

void VjObs_i::setPostdataState(CORBA::Short value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setPostdataState(CORBA::Short value)>"
}

CORBA::Short VjObs_i::getPostdataState(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Short VjObs_i::getPostdataState()>"
}

void VjObs_i::setPreState(CORBA::Short value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setPreState(CORBA::Short value)>"
}

CORBA::Short VjObs_i::getPreState(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Short VjObs_i::getPreState()>"
}

void VjObs_i::setTimesteps(CORBA::Short value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setTimesteps(CORBA::Short value)>"
}

CORBA::Short VjObs_i::getTimesteps(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Short VjObs_i::getTimesteps()>"
}

void VjObs_i::setNumTeacherArrays(CORBA::Short value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setNumTeacherArrays(CORBA::Short value)>"
}

CORBA::Short VjObs_i::getNumTeacherArrays(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Short VjObs_i::getNumTeacherArrays()>"
}

void VjObs_i::setTeacherState(CORBA::Short value){
  // insert code here and remove the warning
  #warning "Code missing in function <void VjObs_i::setTeacherState(CORBA::Short value)>"
}

CORBA::Short VjObs_i::getTeacherState(){
  // insert code here and remove the warning
  #warning "Code missing in function <CORBA::Short VjObs_i::getTeacherState()>"
}



// End of example implementational code



int main(int argc, char** argv)
{
  try {
    // Initialise the ORB.
    CORBA::ORB_var orb = CORBA::ORB_init(argc, argv);

    // Obtain a reference to the root POA.
    CORBA::Object_var obj = orb->resolve_initial_references("RootPOA");
    PortableServer::POA_var poa = PortableServer::POA::_narrow(obj);

    // We allocate the objects on the heap.  Since these are reference
    // counted objects, they will be deleted by the POA when they are no
    // longer needed.
    VjObs_i* myVjObs_i = new VjObs_i();


    // Activate the objects.  This tells the POA that the objects are
    // ready to accept requests.
    PortableServer::ObjectId_var myVjObs_iid = poa->activate_object(myVjObs_i);


    // Obtain a reference to each object and output the stringified
    // IOR to stdout
    {
      // IDL interface: VjObs
      CORBA::Object_var ref = myVjObs_i->_this();
      CORBA::String_var sior(orb->object_to_string(ref));
      cout << "IDL object VjObs IOR = '" << (char*)sior << "'" << endl;
    }



    // Obtain a POAManager, and tell the POA to start accepting
    // requests on its objects.
    PortableServer::POAManager_var pman = poa->the_POAManager();
    pman->activate();

    orb->run();
    orb->destroy();
  }
  catch(CORBA::SystemException&) {
    cerr << "Caught CORBA::SystemException." << endl;
  }
  catch(CORBA::Exception&) {
    cerr << "Caught CORBA::Exception." << endl;
  }
  catch(omniORB::fatalException& fe) {
    cerr << "Caught omniORB::fatalException:" << endl;
    cerr << "  file: " << fe.file() << endl;
    cerr << "  line: " << fe.line() << endl;
    cerr << "  mesg: " << fe.errmsg() << endl;
  }
  catch(...) {
    cerr << "Caught unknown exception." << endl;
  }

  return 0;
}

