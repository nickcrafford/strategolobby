echo "Starting Test Server"
echo ""
cd ../mochiweb/strategoserver/
./start-test.sh
cd ../
cd ../test/
sleep 2
echo "Running Tests"
echo ""
python2.6 tests.py
echo ""
echo "Stopping Test Server"
echo ""
cd ../mochiweb/strategoserver/
./stop-test.sh
cd ../
cd ../test/
