./build.sh
if [ $? -eq 0 ]; then
    echo "Compilation succeded, rendering..."
    time ./rayhs data/texture.json
    open out.ppm
else
    echo "Compilation failed!"
fi
