/*
 *  vtk_streamlines.h
 *  
 *
 *  Created by mccdo on 7/18/09.
 *  Copyright 2009 Iowa State University. All rights reserved.
 *
 */


streamTracer = vtkStreamTracer::New();
integ = vtkRungeKutta4::New();

vtkCellDataToPointData* c2p = vtkCellDataToPointData::New();
c2p->SetInput(  GetActiveDataSet()->GetDataSet() );

streamTracer->SetInputConnection( c2p->GetOutputPort() );
//overall length of streamline
streamTracer->SetMaximumPropagation( propagationTime );

// typically < 1
streamTracer->SetMaximumIntegrationStep( integrationStepLength );

streamTracer->SetIntegrationDirectionToBoth();

streamTracer->SetSource( seedPoints );

streamTracer->SetIntegrator( integ );
streamTracer->ComputeVorticityOn();
streamTracer->SetInputArrayToProcess( 0, 0, 0,
                                     vtkDataObject::FIELD_ASSOCIATION_POINTS, 
                                     GetActiveDataSet()->GetActiveVectorName().c_str() );

vtkCleanPolyData* cleanPD = vtkCleanPolyData::New();
cleanPD->PointMergingOn();
cleanPD->SetInputConnection( streamTracer->GetOutputPort() );
{
vtkXMLPolyDataWriter* writer = vtkXMLPolyDataWriter::New();
writer->SetInput( cleanPD->GetOutput() );
//writer->SetDataModeToAscii();
writer->SetFileName( "streamline_output.vtp" );
writer->Write();
writer->Delete();
}
mapper->SetInputConnection( cleanPD->GetOutputPort() );

