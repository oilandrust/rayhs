./build.sh
if [ $? -eq 0 ]; then
    echo "Compilation succeded, rendering..."
    ./rayTracer
    open out.ppm
else
    echo "Compilation failed!"
fi