<template>
  <page-skeleton
    v-loading.fullscreen.lock="submitting"
    element-loading-text="saving">
    <template #breadcrumbs>
      <el-breadcrumb-item :to="{ name: 'recipe-list' }">
        My Recipes
      </el-breadcrumb-item>
      <el-breadcrumb-item :to="{ name: 'recipe-add' }">
        Add Recipe
      </el-breadcrumb-item>
    </template>
    <el-form ref="form" :model="form">
      <el-form-item label="Name" prop="name"
        :rules="[{
          required: true,
          message: 'Please input recipe name',
          trigger: 'blur'
        }]">
        <el-input size="large" v-model="form.name" />
      </el-form-item>
      <el-form-item label="Tags" prop="tags">
        <el-tooltip class="tooltip" effect="dark"
          placement="bottom" content="comma separated list">
          <el-input size="large" v-model="form.tags" />
        </el-tooltip>
      </el-form-item>
    </el-form>
    <image-list v-model="fileList" />
    <el-button
      type="primary"
      size="large"
      class="create-button"
      :disabled="submitting"
      circle
      @click="onSubmit">
      <el-icon><i-mdi-content-save /></el-icon>
    </el-button>
  </page-skeleton>
</template>

<script>
import PageSkeleton from "./PageSkeleton.vue"
import ImageList from "./ImageList.vue"
import { postRecipe } from "../Api.js"

export default {
  name: 'RecipeAdd',
  components: {PageSkeleton, ImageList},
  data() {
    return {
      form: {
        name: "",
        tags: "",
      },
      fileList: [],
      submitting: false,
    }
  },
  methods: {
    async onSubmit() {
      if (this.$data.submitting) {
        return
      }
      this.$data.submitting = true

      let isValid = false

      try {
        isValid = await this.$refs.form.validate()
      } catch (validationError) {
        ElMessage.error(Object.values(validationError).flat()
          .map(e => e.message).join("\n"))
      }

      if (isValid) {
        try {
          await postRecipe(
            this.$data.form.name, 
            this.$data.form.tags,
            this.$data.fileList.map(f => f.file)
          )
          this.$router.push({ name: "recipe-list" })
        } catch (err) {
          console.error(err)
          ElMessage.error("An error occured uploading the recipe. Check your network")
        }
      }
      this.$data.submitting = false
    },
  },
}
</script>

<style>
.create-button {
  font-size: 1.6em;
  /* Sets it to work based on the font-size of the icon */
  --el-button-size: auto;
  position: fixed;
  bottom: 32px;
  right: 32px;
}
</style>
