<template>
  <div>
    <div>
      <el-row v-for="(image, idx) in modelValue" :key="image.url">
        <el-col :span="2" :md="{span: 2, offset: 2}">
          <div class="image-buttons">
            <el-button type="text" round
              @click="moveImage(idx, idx-1)"
              :disabled="idx == 0">
              <el-icon><i-ep-caret-top /></el-icon>
            </el-button>
            <el-button type="text" round @click="deleteImage(idx)">
              <el-icon><i-ep-delete-filled /></el-icon>
            </el-button>
            <el-button type="text" round
              @click="moveImage(idx, idx+1)"
              :disabled="idx == (modelValue.length - 1)">
              <el-icon><i-ep-caret-bottom /></el-icon>
            </el-button>
          </div>
        </el-col>
        <el-col :span="22" :md="{span: 18, offset: 0}">
          <el-image :src="image.url" />
        </el-col>
      </el-row>
    </div>
    <el-upload
      class="image-upload"
      drag
      action="#"
      :auto-upload="false"
      :on-change="addNewImage"
      :show-file-list="false"
      >
      <el-icon class="el-icon--upload">
        <i-ep-upload-filled />
      </el-icon>
      <div class="el-upload__text">
        Drop file here or <em>click to upload</em>
      </div>
    </el-upload>
  </div>
</template>

<script>
export default {
  name: "ImageList",
  props: {
    // An array of objects { url: String, file: File? }
    // The url is required, but file is not.
    modelValue: {
      type: Array,
      required: true,
    },
  },
  emits: ["update:modelValue"],
  methods: {
    deleteImage(idx) {
      let images = Array.from(this.modelValue)
      images.splice(idx, 1)
      this.$emit("update:modelValue", images)
    },
    moveImage(from, to) {
      let images = Array.from(this.modelValue)
      let img = images[from]
      images[from] = images[to]
      images[to] = img
      this.$emit("update:modelValue", images)
    },
    addNewImage(file, fileList) {
      let images = Array.from(this.modelValue)
      images.push({
        url: URL.createObjectURL(file.raw),
        file: file.raw,
      })
      this.$emit("update:modelValue", images)
    },
    // This loads the file objects for any images that don't have them
    // already. Returns an image array ala the images prop.
    async loadFiles() {
      return Promise.all(this.modelValue.map(async (imageFile) => {
        if (!imageFile.file) {
          let response = await fetch(imageFile.url)
          return response.blob()
        } else {
          return imageFile.file
        }
      }))
    },
  },
}
</script>

<style>
.image-buttons {
  display: flex;
  flex-flow: column nowrap;
  align-items: center;
}
.image-buttons .el-button {
  margin: 0px;
}
.image-buttons .el-button .el-icon {
  font-size: 20px;
}
</style>
