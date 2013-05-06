
SignalMaker <- StringSignal;
SignalMaker.RegisterSignal( "VESLoadFile", "FileLoader" );
FileLoader.signal( "Surface0.75.stl" );
FileLoader.signal( "eightCorners.stl" );
FileLoader.signal( "3scl2vec.vtu" );
