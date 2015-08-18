./build.sh
if [ $? -eq 0 ]; then
    echo "Compilation succeded, rendering..."
    time ./rayhs -otexture.ppm data/texture.json
    open out.ppm
    time ./rayhs -ooutScene.ppm data/outScene.json
    open outScene.ppm
    time ./rayhs -ocornell.ppm data/cornellBox.json
    open cornell.ppm
    time ./rayhs -odragon.ppm data/dragon.json
    open dragon.ppm
else
    echo "Compilation failed!"
fi
