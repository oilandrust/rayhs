./build.sh
if [ $? -eq 0 ]; then
    echo "Compilation succeded, rendering..."
    time ./rayhs data/outScene.json
    open out.ppm
else
    echo "Compilation failed!"
fi
