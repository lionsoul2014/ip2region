/**
 * Async For
 * @param {Array} groupArray 
 * @param {Function} exeCallBack 
 * @param {Function} finalCallBack 
 */
function asyncFor(groupArray, exeCallBack, finalCallBack) {

    let i = 0;

    function _innerAsyncLoop() {
        if (i < groupArray.length) {
            exeCallBack(groupArray[i++], _innerAsyncLoop);
        }
        else {
            finalCallBack();
        }
    }

    _innerAsyncLoop();
}

module.exports = asyncFor;