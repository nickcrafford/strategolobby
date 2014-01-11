echo "Starting Test Server"
echo ""
cd ../mochiweb/strategoserver/
./start-test.sh
cd ../
cd ../test/
sleep 2
echo "Running Tests"
echo ""
while true
do
python2.6 tests.py
done
echo ""
echo "Stopping Test Server"
echo ""
cd ../mochiweb/strategoserver/
./stop-test.sh
cd ../
cd ../test/
